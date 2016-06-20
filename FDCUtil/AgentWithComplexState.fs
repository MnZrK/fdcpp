module FDCUtil.AgentWithComplexState

open System.Threading
open System.Threading.Tasks

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
| Die of AsyncReplyChannel<unit>

type T<'action, 'state, 'e> = {
    post: 'action -> unit
    post_and_reply: 'action -> Result<'state, ReplyError<'e>>
    state_changed: IEvent<'state * 'state>
    fetch: unit -> Result<'state, FetchError>
}

let fire_event (event: Event<'a>) x = 
    Task.Run(fun () -> event.Trigger(x)) // WILL are we sure it will never-ever block our thread?

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
            // WILL add error_unhandled event and push all errors there ?
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
            | Die replychannel ->
                stopped.Cancel()
                replychannel.Reply(())
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

    let stop () = agent.PostAndReply Die

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
