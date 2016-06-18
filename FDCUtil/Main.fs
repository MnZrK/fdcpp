module FDCUtil.Main

open System

module Array =
    let concatSplit eom_marker chunks = 
        let rec loop_find_eom loop acc msg = seq {
            let i_maybe = Array.tryFindIndex ((=) eom_marker) msg
            match i_maybe with
            | None ->
                yield! loop (msg::acc)
            | Some i ->
                if i < (msg.Length - 1) then
                    yield
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    yield! loop_find_eom loop [] msg.[(i+1)..(msg.Length-1)]
                else
                    yield
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    yield! loop []
        }
        let rec loop acc = seq {
            for chunk in chunks do yield! loop_find_eom loop acc chunk
        }
        loop []

let concat_and_split_stream buffer_size eom_marker (stream: IO.Stream) =
    let rec read() = seq {
        let buffer = Array.zeroCreate buffer_size
        let bytes_read = 
            try
                stream.Read(buffer, 0, buffer_size)
            with 
            | :? IO.IOException
            | :? ObjectDisposedException ->
                0
        if bytes_read <= 0 then 
            ()
        else 
            yield buffer.[0..bytes_read-1]
            yield! read()
    }
    Array.concatSplit eom_marker (read()) 

/// Do not use directly
type MapWithArbKeyType<'key, 'value> when 'key : comparison = MapWithArbKeyType of Map<'key, 'value>
[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>] 
type MapWithArbKey<'key, 'value> when 'key : comparison (construct_key: 'value -> 'key) = 
    member __.add value (MapWithArbKeyType map) = 
        let key = construct_key value
        MapWithArbKeyType (Map.add key value map) 

    member __.addIfMissing value (MapWithArbKeyType map) =
        let key = construct_key value
        if Map.containsKey key map then map
        else Map.add key value map
        |> MapWithArbKeyType

    member __.find key (MapWithArbKeyType map) = 
        Map.find key map

    member __.tryFind key (MapWithArbKeyType map) =
        Map.tryFind key map

    member __.containsKey key (MapWithArbKeyType map) = 
        Map.containsKey key map

    member __.empty = 
        MapWithArbKeyType Map.empty<'key, 'value>

    member __.remove key (MapWithArbKeyType map) = 
        Map.remove key map |> MapWithArbKeyType

module String =
  let split (sep:string) (str:string) =
    match sep, str with
    | ((null | ""), _) | (_, (null | "")) -> seq [str]
    | _ ->
      let n, j = str.Length, sep.Length
      let rec loop p = 
        seq {
          if p < n then
            let i = match str.IndexOf(sep, p) with -1 -> n | i -> i
            yield str.Substring(p, i - p)
            yield! loop (i + j)
        }
      loop 0

let ct a _ = a

let callable_once fn =
    let lazyf = lazy (fn())
    fun () -> lazyf.Value

    // let executed = ref false
    // (fun () ->
    //     if not !executed then
    //         lock executed (fun () ->
    //             if not !executed then
    //                 executed := true
    //                 fn()
    //         ) 
    //     )

let tee fn x = x |> fn |> ignore; x
let ( |>! ) x fn = tee fn x

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a,'b) result =
| Success of 'a
| Failure of 'b
type Result<'a, 'b> = result<'a, 'b> 

module Result =
    let fromOption o e =
        match o with 
        | None -> Failure e
        | Some x -> Success x
    let toOption r =
        match r with
        | Failure _ -> None
        | Success x -> Some x

    // type Aggregated<'a, 'b> =
    // | Inner of 'a
    // | Outer of 'b

    // let flattenSuccess (mm: Result<Result<'a,'c>,'b>) =
    //     match mm with
    //     | Failure e -> Outer e |> Failure
    //     | Success m ->
    //         match m with
    //         | Failure e -> Inner e |> Failure
    //         | Success x -> Success x

    let isSuccess = 
        function
        | Success _ -> true
        | Failure _ -> false

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

    let fork successF failureF m = 
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

    let collect f m = bind m f 

    type SuccessBuilder() =
        member __.Bind(m, f) = bindSuccess m f
        member __.Return(x) = Success x
        member __.ReturnFrom(x) = x
    type SuccessWithStringFailureBuilder() =
        let stringify (x: obj) =
            match x with
            | :? string as str -> str
            | x -> sprintf "%A" x

        member __.Bind(m, f) =
            (m |> mapFailure stringify) 
        >>= (f >> mapFailure stringify)
        
        member __.Return(x) = Success x
        member __.ReturnFrom(x) = x |> mapFailure stringify 
    let success_workflow = new SuccessBuilder()
    let success_workflow_with_string_failures = new SuccessWithStringFailureBuilder()

    type FailureBuilder() =
        member __.Bind(m, f) = bindFailure m f
        member __.Return(x) = Failure x
        member __.ReturnFrom(x) = x
    let failure_workflow = new FailureBuilder()

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a, 'b) resultlist = result<'a, 'b> list
type ResultList<'a, 'b> = resultlist<'a, 'b> 
module ResultList =
    let swap (xs: resultlist<'a, _>): result<'a list, _> =
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

    let bind (m: result<'x list, _>) (f: 'x -> result<'x list, _>): result<'x list, _> =
        match m with
        | Success xs ->
            xs
            |> List.map f 
            |> swap 
            |> Result.map List.concat
        | Failure x -> 
            Failure x 
    let (>>=) = bind

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a, 'b) asyncresult = Async<result<'a,'b>>
type AsyncResult<'a, 'b> = asyncresult<'a, 'b>
module AsyncResult = 
    let map f m = async {
        let! res = m
        return Result.map f res
    } 
    let (<?>) = map

    let bindSuccess m f = async {
        let! res = m
        return! 
            match res with
            | Failure x ->  async { return Failure x }
            | Success x -> f x
    }
    let bind = bindSuccess
    let (>>=) = bind

    type SuccessBuilder() =
        member __.Bind(m, f) = bindSuccess m f
        member __.Return(x) = async { return Success x }
        member __.ReturnFrom(x) = x
    let success_workflow = new SuccessBuilder()    

module AgentWithComplexState =
    open System.Threading

    type ReplyError<'e> = 
    | IsStopped
    | ActionFailure of 'e
    | ActionException of System.Exception

    type FetchError =
    | IsStopped

    type internal Message<'action, 'state, 'e> = 
    | Post of 'action
    | PostAndReply of 'action * AsyncReplyChannel<Result<'state, ReplyError<'e>>>
    | Fetch of AsyncReplyChannel<'state>
    | Die

    type T<'action, 'state, 'e> = {
        post: 'action -> unit
        post_and_reply: 'action -> Result<'state, ReplyError<'e>>
        state_changed: IEvent<'state * 'state>
        fetch: unit -> Result<'state, FetchError>
    }

    let fire_event (event: Event<'a>) x = 
        Tasks.Task.Run(fun () -> event.Trigger(x)) // are we sure it will never-ever block our thread?

    let fire_and_forget_event event x = fire_event event x |> ignore 

    let internal _create (state, deps) f =
        let state_changed = new Event<('state*'deps) * ('state*'deps)>()

        let stopped = new CancellationTokenSource()

        let process_post x (acc_state, acc_deps) = 
            let fResult = 
                try
                    f x (acc_state, acc_deps) |> Result.get |> Some
                with
                | _ -> None

            match fResult with
            | Some (acc_state', acc_deps') ->
                if (acc_state' <> acc_state) then
                    let full_state = acc_state, acc_deps
                    let full_state' = acc_state', acc_deps'
                    fire_and_forget_event state_changed ((full_state, full_state'))
                (acc_state', acc_deps')
            | _ ->
                // swallow the error ... nowhere to return it
                // TODO add error_unhandled event and push all errors there ?
                //  so we can at least log them when they happen
                (acc_state, acc_deps)

        let process_post_and_reply x (replyChannel: AsyncReplyChannel<Result<'state*'deps, ReplyError<'e>>>) (acc_state, acc_deps) =
            let fResult =          
                try
                    match f x (acc_state, acc_deps) with
                    | Success x -> Success x
                    | Failure e -> ReplyError.ActionFailure e |> Failure
                with
                | ex ->
                    ReplyError.ActionException ex |> Failure
                    
            replyChannel.Reply(fResult)

            match fResult with
            | Success (acc_state', acc_deps') ->
                if (acc_state' <> acc_state) then
                    let full_state = acc_state, acc_deps
                    let full_state' = acc_state', acc_deps'
                    fire_and_forget_event state_changed ((full_state, full_state'))
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
                    replychannel.Reply((acc_state, acc_deps))
                    return! loop (acc_state, acc_deps)
                | Die ->
                    stopped.Cancel()
                    return ()
            }
            loop (state, deps)
        )

        let post = Post >> agent.Post
        let post_and_reply x = 
            
            let wait = agent.PostAndAsyncReply(fun reply -> PostAndReply (x, reply))
            let task = Async.StartAsTask(wait, cancellationToken = stopped.Token)
            
            let res = 
                try
                    Async.AwaitTask task |> Async.RunSynchronously
                with 
                | :? System.OperationCanceledException -> 
                    ReplyError.IsStopped |> Failure
            res

        let fetch () = 
            let wait = agent.PostAndAsyncReply(fun reply -> Fetch (reply))
            let task = Async.StartAsTask(wait, cancellationToken = stopped.Token)
            let res = 
                try
                    Async.AwaitTask task |> Async.RunSynchronously |> Success
                with 
                | :? System.OperationCanceledException -> 
                    FetchError.IsStopped |> Failure
            res

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
