[<AutoOpen>]
module FDCDomain.DomainLogic

open System
open System.Threading

open FSharp.Control.Reactive
// open FSharpx.Control

open FDCUtil

let UserMap = new MapWithArbKey<NickData.T, User>(fun user -> user.nick)
// domain logic (functions)
[<AutoOpen>]
module UserModule = 
    let create_user nick = 
        { nick = nick; user_info = None; is_Op = false }

    let add_nick nick' users =
        if UserMap.containsKey nick' users then users
        else UserMap.add (create_user nick') users

    let add_nicks nicks =
        nicks |> Seq.map create_user |> Seq.fold (fun map user -> UserMap.add user map) UserMap.empty

    let remove_nick nick users =
        UserMap.remove nick users

    let add_nick_with_info nick user_info users =
        let user' = 
            match UserMap.tryFind nick users with
            | Some user -> { user with user_info = user_info } 
            | None -> { nick = nick; user_info = user_info; is_Op = false }
        UserMap.add user' users

    let mark_nick_as_op nick users =
        let user' = 
            match UserMap.tryFind nick users with
            | Some user -> { user with is_Op = true } 
            | None -> { nick = nick; user_info = None; is_Op = true }
        UserMap.add user' users

let private validate_state (state, deps) =
    match (state, deps) with
    | NotConnected, _ ->
        Success ()
    | _, None ->
        // this is a programming error if we get here. should never-ever happen
        Failure InvalidState
    | _ ->
        Success ()

let private dispatch_send_action action (state, deps) =
    match action, state with
    | Search action, LoggedIn env ->
        let msg = DcppSendMessage.Search { listen_info = action.listen_info; search_str = action.search_str }

        deps.transport.Write msg

        state |> Success
    | SendSupports, LoggedIn env ->
        deps.transport.Write Supports
        state |> Success
    | ConnectToMe action, LoggedIn env ->
        let msg = DcppSendMessage.ConnectToMe { listen_info = action.listen_info; remote_nick = action.remote_nick }

        deps.transport.Write msg

        state |> Success
    | SendNick (nick, key), Connected env ->
        let msg = ValidateNick { nick = nick; key = key }

        deps.transport.Write msg

        let state' = WaitingForAuth {
            connect_info = env.connect_info
            nick = nick
            key = key
        }
        Success state'
    | SendPass pass, WaitingForAuth env ->
        let msg = MyPass { password = pass }

        deps.transport.Write msg

        let state' = WaitingForPassAuth {
            connect_info = env.connect_info
            nick = env.nick
            password = pass
        }
        Success state'
    | RetryNick nick', WaitingForAuth env ->
        let msg = DcppSendMessage.ValidateNick {
            nick = nick'
            key = env.key
        }

        deps.transport.Write msg

        let state' = WaitingForAuth { env with nick = nick' }
        Success state'
    | _, _ ->
        Failure InvalidAction

let private dispatch_main_action action env =
    match action with
    | MyInfoed (nick', nick_info') ->
        let nicks' = (add_nick_with_info nick' (Some nick_info') env.users)
        Success { env with users = nicks' }

    | Quitted nick' ->
        Success { env with users = (remove_nick nick' env.users) }

    | NickListed nicks' ->
        Success { env with users = add_nicks nicks' }
    | OpListed nicks->
        let users' = nicks |> Seq.fold (fun nicks' nick -> mark_nick_as_op nick nicks') env.users
        Success { env with users = users' } 

let private dispatch_helloed_message nick' (state, deps_maybe) = Result.success_workflow {
    match state with 
    | WaitingForAuth { nick = nick }
    | WaitingForPassAuth { nick = nick }
        when nick' <> nick ->
            return state
    | WaitingForAuth { connect_info = ci; nick = nick }
    | WaitingForPassAuth { connect_info = ci; nick = nick } ->
        let myinfo_msg = DcppSendMessage.MyInfo {nick = nick; share_size = PositiveInt.fromULong 0UL}
        // we are almost logged in, just need to send $Version and $MyINFO
        let! deps = Result.ofOption DepsAreMissing deps_maybe
        deps.transport.Write DcppSendMessage.Version
        deps.transport.Write myinfo_msg

        // TODO fix (make us logged in when server responds with HubName or HubTopic or whatever)
        Thread.Sleep 5000

        return LoggedIn { connect_info = ci; nick = nick; users = UserMap.empty }
    | LoggedIn env ->
        return LoggedIn { env with users = (add_nick nick' env.users) }
    | _ ->
        return! Failure InvalidAction
}

let private dispatch_action (log: ILogger) (create_transport: CreateTransport) action (state, deps_maybe) =
    // log.Trace "Dispatching action %A" action

    Result.success_workflow {
        do! validate_state (state, deps_maybe)
        match action, state with
        | Disconnected, _ ->
            // WILL what if somebody posts Disconnected event instead of Disconnect? then we might get into infinite loop 
            //  of constant connecting-disconnecting
            deps_maybe
            |> Option.map (fun deps -> deps.transport.Dispose())
            |> ignore

            return (NotConnected, None)
        | Disconnect, _ ->
            match deps_maybe with
            | None -> 
                return (NotConnected, None)
            | Some deps ->
                log.Info "Disconnecting..."
                // WILL we rely on the fact that disposing transport will call onCompletion handler
                //  where we send Disconnected event and that in turn changes state. But all of it 
                //  enforces strong coupling between dispatch_action and start_queue which is kinda bad.
                deps.transport.Dispose()
                // i dont like that state here is not changing, but 
                //  introducing Disconnecting state just for this 
                //  seems like overkill.
                return (state, deps_maybe)  

        | Send send_action, _ ->
            let! deps = Result.ofOption DepsAreMissing deps_maybe
            let! state' = dispatch_send_action send_action (state, deps)
            return state', deps_maybe

        | Connect ci, NotConnected ->
            let! transport =
                create_transport ci
                |> Result.mapFailure ActionError.TransportError

            let state' = Connected { connect_info = ci }

            let deps' =
                match deps_maybe with
                | None -> { transport = transport }
                | Some deps -> { deps with transport = transport }

            return state', Some deps'

        | Helloed nick, _ ->
            let! state' = dispatch_helloed_message nick (state, deps_maybe)
            return state', deps_maybe

        | Main m_action, LoggedIn env ->
            let! env' = dispatch_main_action m_action env
            return LoggedIn env', deps_maybe 

        | _, _ ->
            return! Failure InvalidAction
    }
    |> Result.mapFailure (fun e -> e, action, state)
    |>! Result.mapFailure (fun x -> log.Error "Error while dispatching action: %A" x)

let private handle_received_message (log: ILogger) pass_data_maybe (agent: Agent) (env: ReceivedHandlerEnv) dcpp_msg =
    // log.Trace "Received message %A" dcpp_msg
    match dcpp_msg with
    | DcppReceiveMessage.Lock msg ->
        agent.post << Send <| SendNick (env.nick, KeyData.create msg.lock)
        env
    | ValidateDenied ->
        let nick' =
            env.nick
            |> NickData.unwrap
            |> ASCIIString.unwrap
            |> (+) <| "1"
            |> NickData.create
            |>! Result.mapFailure (fun e -> log.Error "Could not create new nick from old nick %A: %A" env.nick e)
            |>! Result.map (fun nick' -> agent.post << Send << RetryNick <| nick')
            |> Result.fork id (ct env.nick)
        { env with nick = nick' }
    | Hello msg ->
        log.Info "User %A has enterred" msg.nick
        let res =
            agent.post_and_reply << Helloed <| msg.nick
            |> (Result.fork
                <| ignore
                <| (fun e ->
                    // TODO fix this will trigger even for usual Hellos, not for the one which confirms our login
                    log.Error "Could not finish logging in, disconnecting: %A" e
                    agent.post <| Disconnect
                    ))
        env
    | DcppReceiveMessage.MyInfo msg ->
        agent.post << Main <| MyInfoed (msg.nick, { share_size = msg.share_size })
        env
    | Quit msg ->
        log.Info "User %A has quitted" msg.nick
        agent.post << Main << Quitted <| msg.nick
        env
    | GetPass ->
        match pass_data_maybe with
        | None ->
            // TODO terminate everything somehow ?
            log.Error "Server asks for password but we don't have any"
        | Some pass_data ->
            agent.post << Send <| SendPass pass_data
        env
    | BadPass ->
        log.Error "BadPass for nick %A" env.nick
        agent.post AgentAction.Disconnect
        env
    | DcppReceiveMessage.LoggedIn ->
        // WILL it is related to Op users, we dont care about them for a moment
        env
    | ChatMessage msg ->
        log.Trace "Chat message from %A: %s" msg.nick msg.message
        env
    | HubTopic msg ->
        log.Info "Hub topic: %s" msg.topic
        env
    | HubName msg ->
        log.Info "Hub name: %s" msg.name
        env
    | NickList msg ->
        agent.post << Main <| NickListed msg.nicks
        env
    | OpList msg ->
        agent.post << Main <| OpListed msg.nicks
        env
    | DcppReceiveMessage.Key _
    | DcppReceiveMessage.MyNick _
    | DcppReceiveMessage.SR _
    | DcppReceiveMessage.Direction _ ->
        log.Error "Did not expect to receive client-only message from server: %A" dcpp_msg
        env
    | IgnoreIt ->
        env

let private handle_agent (log: ILogger) callback connect_info (nick_data, pass_data_maybe) (agent: Agent) =
    log.Trace "We are inside agent now!"

    let handle_received_message_applied = handle_received_message log pass_data_maybe agent

    // data transformation for convenience
    let full_state_events = agent.state_changed
    let state_events =
        agent.state_changed
        |> Observable.map (fun ((state, _), (state', _)) -> (state, state'))
        // |>! Event.add (fun (state, state') -> log.Trace "State changed from %A to %A" state state')

    full_state_events |> Observable.add (
        function
        | (NotConnected, _), (Connected ci, Some deps) ->
            log.Info "Connected to %A" ci

            // handling received dcpp messages
            deps.transport.Received
            |> Control.Observable.scan handle_received_message_applied { nick = nick_data }
            |> Observable.subscribeWithCompletion ignore (fun () ->
                log.Warn "Disconnected from %A" ci
                agent.post Disconnected
            )
            |> ignore
        | _ -> ()
    )

    state_events |> Observable.add (
        function
        | (WaitingForAuth _ | WaitingForPassAuth _), LoggedIn env' ->
            log.Info "Successfully logged in as %A" env'.nick
        | _, NotConnected ->
            log.Info "Disconnected"
        | _ -> ()
    )

    use stop_reconnecting = 
        state_events |> Observable.subscribe (
            function
            | _, NotConnected ->
                log.Info "Connecting to %A..." connect_info
                agent.post_and_reply <| Connect connect_info
                |> Result.mapFailure (log.Error "Couldnt connect %A")
                |> Result.map (log.Info "Reconnected %A")
                |> ignore
            | _ -> ()
        )

    agent.errored |> Observable.add (log.Error "Action error: %A")

    log.Info "Connecting to %A..." connect_info
    agent.post_and_reply <| Connect connect_info
    |> Result.mapFailure CouldntConnect
    |> Result.map (fun _ ->
        try
            callback(agent)
        finally
            stop_reconnecting.Dispose()
            agent.post_and_reply <| Disconnect |> ignore
    )

let start_queue (log: ILogger) (create_transport: CreateTransport) await_terminator connect_info (nick_data, pass_data_maybe) =
    log.Info "Starting queue..."

    let dispatch_action_applied = dispatch_action log create_transport
    let handle_agent_applied = handle_agent log await_terminator connect_info (nick_data, pass_data_maybe)

    AgentWithComplexState.loop
    <| (State.NotConnected, None)
    <| dispatch_action_applied
    <| handle_agent_applied
