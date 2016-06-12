module FDCUtil.Main

type Result<'a, 'b> = 
| Success of 'a
| Failure of 'b
module Result = 
    let mapSuccess f =
        function
        | Success x -> f x |> Success
        | Failure x -> Failure x
    let map = mapSuccess
    let (<?>) = map

    let mapFailure f =
        function
        | Success x -> Success x
        | Failure x -> f x |> Failure

    let bindSuccess m f = 
        match m with
        | Success x -> f x
        | Failure x -> Failure x
    let bind = bindSuccess
    let (>>=) = bind

    let bindFailure m f = 
        match m with
        | Success x -> Success x
        | Failure x -> f x

    let applySuccess mF m =
        match mF with
        | Success f -> mapSuccess f m
        | Failure x -> Failure x
    let apply = applySuccess
    let (<*>) = apply

    let applyFailure mF m = 
        match mF with
        | Success x -> Success x 
        | Failure f -> mapFailure f m

    let fold successF failureF m = 
        match m with
        | Success x -> successF x
        | Failure x -> failureF x

    let getSuccess m = 
        match m with
        | Success x -> 
            x
        | Failure x -> 
            raise (new System.InvalidOperationException(sprintf "Trying to get Success from Failure: %A" x))
    let get = getSuccess

    let getFailure m = 
        match m with
        | Success x -> 
            raise (new System.InvalidOperationException(sprintf "Trying to get Failure from Success: %A" x))
        | Failure x -> 
            x

    type SuccessBuilder() =
        member this.Bind(m, f) = bindSuccess m f
        member this.Return(x) = Success x
        member this.ReturnFrom(x) = x
    let success_workflow = new SuccessBuilder()

    type FailureBuilder() =
        member this.Bind(m, f) = bindFailure m f
        member this.Return(x) = Failure x
        member this.ReturnFrom(x) = x
    let failure_workflow = new FailureBuilder()

type ResultList<'a, 'b> = Result<'a, 'b> list
module ResultList =
    let swap (xs: ResultList<'a, _>): Result<'a list, _> =
        let error = 
            xs 
            |> List.tryPick (
                function
                | Success x -> None
                | Failure x -> Failure x |> Some
            ) 
        
        match error with
        | Some x -> x
        | None -> 
            xs
            |> List.choose (
                function
                | Failure x -> None
                | Success x -> x |> Some
            )
            |> Success

    let bind (m: Result<'x list, _>) (f: 'x -> Result<'x list, _>): Result<'x list, _> =
        match m with
        | Success xs ->
            xs
            |> List.map f 
            |> swap 
            |> Result.map List.concat
        | Failure x -> 
            Failure x 
    let (>>=) = bind

module AgentWithComplexState =
    open System.Threading

    type Error = 
    | IsStopped
    | OtherError of System.Exception

    type MessageResult<'a> = Result<'a, Error>
    type FetchResult<'state> = MessageResult<'state>
    type ReplyResult<'state, 'e> = MessageResult<Result<'state, 'e>>

    type internal Message<'action, 'state, 'e> = 
    | Post of 'action
    | PostAndReply of 'action * AsyncReplyChannel<ReplyResult<'state, 'e>>
    | Fetch of AsyncReplyChannel<FetchResult<'state>>
    | Die

    type T<'action, 'state, 'e> = {
        post: 'action -> unit
        post_and_reply: 'action -> ReplyResult<'state, 'e>
        state_changed: IEvent<'state * 'state>
        fetch: unit -> FetchResult<'state>
    }

    let internal _create (state, deps) f =
        let state_changed = new Event<('state*'deps) * ('state*'deps)>()

        let stopped = new CancellationTokenSource()

        let process_post x (acc_state, acc_deps) = 
            let fResult = 
                try
                    f x (acc_state, acc_deps) |> Success
                with
                | ex ->
                    Error.OtherError ex |> Failure
                    
            match fResult with
            | Success (Success (acc_state', acc_deps')) ->
                if (acc_state' <> acc_state) then
                    let full_state = acc_state, acc_deps
                    let full_state' = acc_state', acc_deps'
                    state_changed.Trigger((full_state, full_state'))
                (acc_state', acc_deps')
            | _ ->
                // swallow the error ... nowhere to return it
                (acc_state, acc_deps)

        let process_post_and_reply x (replyChannel: AsyncReplyChannel<ReplyResult<'state*'deps, 'e>>) (acc_state, acc_deps) =
            let fResult =          
                try
                    f x (acc_state, acc_deps) |> Success
                with
                | ex ->
                    Error.OtherError ex |> Failure

            replyChannel.Reply(fResult)

            match fResult with
            | Success (Success (acc_state', acc_deps')) ->
                if (acc_state' <> acc_state) then
                    let full_state = acc_state, acc_deps
                    let full_state' = acc_state', acc_deps'
                    state_changed.Trigger((full_state, full_state'))
                (acc_state', acc_deps')
            | _ ->
                (acc_state, acc_deps)


        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec loop (acc_state, acc_deps) = async {
                let! agent_message = inbox.Receive()
                
                match agent_message with
                | Post x -> 
                    return! loop (process_post x (acc_state, acc_deps))
                | PostAndReply (x, replyChannel) ->
                    return! loop (process_post_and_reply x replyChannel (acc_state, acc_deps))
                | Fetch replychannel ->
                    replychannel.Reply(Success (acc_state, acc_deps))
                    return! loop (acc_state, acc_deps)
                | Die ->
                    stopped.Cancel()
                    return ()
            }
            loop (state, deps)
        )

        let create_'post_and_reply' rc = 
            let wait = agent.PostAndAsyncReply(rc)
            let task = Async.StartAsTask(wait, cancellationToken = stopped.Token)
            let res = 
                try
                    Async.AwaitTask task |> Async.RunSynchronously
                with 
                | :? System.OperationCanceledException -> 
                    Error.IsStopped |> Failure
                | ex ->
                    OtherError ex |> Failure
            res

        let post = Post >> agent.Post
        let post_and_reply x = create_'post_and_reply' (fun reply -> PostAndReply (x, reply))
        let fetch () = create_'post_and_reply' Fetch
        let stop () = agent.Post Die

        agent,
        stop,
        { 
            post = post
            post_and_reply = post_and_reply
            fetch = fetch
            state_changed = state_changed.Publish
        }

    let loop full_state f cb =
        let _agent, stop, agent = _create full_state f  
        
        let disposable = {
            new System.IDisposable with 
                member x.Dispose() = 
                    stop()
                    (_agent :> System.IDisposable).Dispose()
            } 

        using disposable (fun _ -> cb agent)        

module Regex =

    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        match input with
        | null -> 
            None
        | _ ->
            let m = Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None
