module FDCUtil.AgentWithComplexState

open System
open System.Threading
open System.Threading.Tasks

open FSharp.Control.Reactive
open FSharpx.Control

type ActionError<'e> =
| ActionFailure of 'e
| ActionException of System.Exception

type ReplyError<'e> = 
| IsStopped
| ActionError of ActionError<'e>

type FetchError =
| IsStopped

type internal Message<'action, 'state, 'e> = 
| Post of 'action
| PostAndReply of 'action * AsyncReplyChannel<Result<'state, ActionError<'e>>>
| Fetch of AsyncReplyChannel<'state>
| Die of AsyncReplyChannel<unit>

type T<'action, 'state, 'e> = {
    post: 'action -> unit
    post_and_reply: 'action -> Result<'state, ReplyError<'e>>
    state_changed: IObservable<'state * 'state>
    errored: IObservable<ActionError<'e>>
    fetch: unit -> Result<'state, FetchError>
}

let private fire_event (subject: Subject<_>) x = subject.OnNext x

let private process_post fire_error fire_state_changed f x (acc_state, acc_deps) = 
    let fResult = 
        try
            f x (acc_state, acc_deps)
            |> Result.mapFailure ActionFailure
        with ex ->
            ActionException ex |> Failure 

    match fResult with
    | Success (acc_state', acc_deps') ->
        if (acc_state' <> acc_state) then
            let full_state = acc_state, acc_deps
            let full_state' = acc_state', acc_deps'
            fire_state_changed ((full_state, full_state'))
        (acc_state', acc_deps')
    | Failure x ->
        fire_error x
        (acc_state, acc_deps)

// it is fine to have a lot of parameters for functions with "dependency injection"
[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NumberOfItems", CheckId = "MaxNumberOfFunctionParameters")>]
let private process_post_and_reply fire_error fire_state_changed f x reply (acc_state, acc_deps) =
    let fResult =          
        try
            match f x (acc_state, acc_deps) with
            | Success x -> Success x
            | Failure e -> ActionFailure e |> Failure
        with
        | ex ->
            ActionException ex |> Failure

    reply(fResult)
    
    match fResult with
    | Success (acc_state', acc_deps') ->
        if (acc_state' <> acc_state) then
            let full_state = acc_state, acc_deps
            let full_state' = acc_state', acc_deps'
            fire_state_changed ((full_state, full_state'))
        (acc_state', acc_deps')
    | Failure x ->
        fire_error x
        (acc_state, acc_deps)

let private agent_loop read_msg process_post process_post_and_reply dispose_events (state, deps) =
    let rec loop (acc_state, acc_deps) = async {
        let! agent_message = read_msg()

        match agent_message with
        | Post x -> 
            return! loop (process_post x (acc_state, acc_deps))
        | PostAndReply (x, replyChannel) ->
            return! loop (process_post_and_reply x replyChannel.Reply (acc_state, acc_deps))
        | Fetch replychannel ->
            replychannel.Reply((acc_state, acc_deps))
            return! loop (acc_state, acc_deps)
        | Die replychannel ->
            dispose_events()

            replychannel.Reply(())
            return ()
    }
    loop (state, deps)
let internal _create (state, deps) f =
    let state_changed = new Subject<('state*'deps) * ('state*'deps)>()
    let dispose_state_changed = lazy (state_changed.OnCompleted())
    let errored = new Subject<ActionError<_>>()
    let dispose_errored = lazy (errored.OnCompleted())

    let stopped = new CancellationTokenSource()
    let dispose_stopped = lazy (
        stopped.Cancel()
        (stopped :> IDisposable).Dispose()
    )

    let fire_error = fire_event errored
    let fire_state_changed = fire_event state_changed

    let dispose_events () = 
        dispose_stopped.Force()
        dispose_errored.Force()
        dispose_state_changed.Force()

    let process_post' = process_post fire_error fire_state_changed f
    let process_post_and_reply' = process_post_and_reply fire_error fire_state_changed f

    let agent = MailboxProcessor.Start(fun inbox -> 
        agent_loop inbox.Receive process_post' process_post_and_reply' dispose_events (state, deps)
    )

    let post = Post >> agent.Post
    let post_and_reply x = 
        
        let wait = agent.PostAndAsyncReply(fun reply -> PostAndReply (x, reply))
        let task = Async.StartAsTask(wait, cancellationToken = stopped.Token)
        
        let res = 
            try
                Async.AwaitTask task |> Async.RunSynchronously
                |> Result.mapFailure ReplyError.ActionError
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

    let stop = 
        (fun () -> agent.PostAndReply Die)
        |> callable_once

    agent,
    stop,
    { 
        post = post
        post_and_reply = post_and_reply
        fetch = fetch
        state_changed = Observable.asObservable state_changed
        errored = Observable.asObservable errored
    }

let loop initial_full_state process_function callback =
    let _agent, stop, agent = _create initial_full_state process_function
    
    let disposable = {
        new System.IDisposable with 
            member x.Dispose() = 
                stop()
                (_agent :> System.IDisposable).Dispose()
        } 

    using disposable (fun _ -> callback agent)        
