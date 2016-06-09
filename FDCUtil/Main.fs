module FDCUtil.Main

type Result<'a, 'b> = 
| Success of 'a
| Failure of 'b
module Result = 
    let mapSuccess f =
        function
        | Success x -> f x |> Success
        | Failure x -> Failure x
    let mapFailure f =
        function
        | Success x -> Success x
        | Failure x -> f x |> Failure

    let bindSuccess m f = 
        match m with
        | Success x -> f x
        | Failure x -> Failure x
    let bindFailure m f = 
        match m with
        | Success x -> Success x
        | Failure x -> f x

    type SuccessBuilder() =
        member this.Bind(m, f) = bindSuccess m f
        member this.Return(x) = Success x
        member this.ReturnFrom(x) = x
    let successWorkflow = new SuccessBuilder()

    type FailureBuilder() =
        member this.Bind(m, f) = bindFailure m f
        member this.Return(x) = Failure x
        member this.ReturnFrom(x) = x
    let failureWorkflow = new FailureBuilder()

module Agent =
    type Message<'a, 'b> = 
    | Post of 'a
    | PostAndReply of 'a * AsyncReplyChannel<'b * 'b>
    | Fetch of AsyncReplyChannel<'b>
    // | Die

    type T<'a, 'b> = {
        post: 'a -> unit
        postAndReply: 'a -> ('b * 'b)
        postAndReplyAsync: 'a -> Async<'b * 'b>
        fetch: unit -> 'b
        fetchAsync: unit -> Async<'b>
        event: IEvent<'b * 'b>
    }

    // BUG cancelling ctstoken makes `fetch` and `*andReply*` methods to stuck infinitely
    // TODO make proper cancellation 
    let create state f =
        let event = new Event<'b * 'b>()

        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec loop accState = async {
                let! agentMessage = inbox.Receive()
                
                match agentMessage with
                | Post x ->
                    let accState' = f x accState
                    event.Trigger((accState, accState'))
                    return! loop accState'
                | PostAndReply (x, replyChannel) ->
                    let accState' = f x accState
                    replyChannel.Reply((accState, accState'))
                    event.Trigger((accState, accState'))
                    return! loop accState'
                | Fetch replychannel ->
                    replychannel.Reply(accState)
                    return! loop accState
                // | Die ->
                //     return ()
            }
            loop state 
        )

        let post = Post >> agent.Post
        let postAndReply x = 
            agent.PostAndReply(fun reply -> PostAndReply (x, reply))  
        let postAndReplyAsync x = 
            agent.PostAndAsyncReply(fun reply -> PostAndReply (x, reply))
        let fetch () = 
            agent.PostAndReply(fun reply -> Fetch reply) 
        let fetchAsync () = 
            agent.PostAndAsyncReply(fun reply -> Fetch reply)

        { 
            post = post
            postAndReply = postAndReply
            postAndReplyAsync = postAndReplyAsync
            fetch = fetch
            fetchAsync = fetchAsync
            event = event.Publish 
        }
