module FDCDomain.Server

open System.Threading

open FDCUtil.Main
open FDCLogger
open FDCNet
open FDCNet.Util

// TODO think about how to restrain transitions from certain states
type State = 
| NotConnected
| Connected of Tcp.Client
| LockAcquired of Tcp.Client * Dcpp.Message.Lock.LockData.T 
| WaitingForAuth of Tcp.Client * Dcpp.Message.Lock.LockData.T * Dcpp.Message.Hello.NickData
| LoggedIn of Tcp.Client * Dcpp.Message.Lock.LockData.T * Dcpp.Message.Hello.NickData

type ConnectionInfo = {
    host: string
    port: int
}

// TODO add unit tests ?

// active actions
let connectAsync connectinfo state =
    let logger = new Logger() 
    match state with
    | NotConnected ->
        async {
            logger.Info "Connecting (%s:%i) ..." connectinfo.host connectinfo.port
            // TODO check what happens when server is not available. exceptions are not propagated it seems
            let! client = Dcpp.startClientAsync connectinfo.host connectinfo.port
            logger.Info "Connected %s:%i" connectinfo.host connectinfo.port
            
            return Connected client
        } |> Success
    | _ -> 
        "Already connected or connecting" |> Failure

let sendNick nick state = 
    let logger = new Logger() 
    match state with
    | LockAcquired (client, lockData) ->
        let (Dcpp.Message.Hello.NickData nickStr) = nick 
        let sendNickRequest = Array.concat [ getBytes "$Key "; Dcpp.Message.Lock.calculateKey lockData; getBytes ("|$ValidateNick "+nickStr+"|") ]
        
        logger.Info "Sending nick info (%s) ..." nickStr
        client.write sendNickRequest
        logger.Info "Sent nick info"
        
        WaitingForAuth (client, lockData, nick) |> Success
    | _ ->
        "Invalid state for authorization" |> Failure

// passive reactions
let onLockMessage (lockMsg: Dcpp.Message.Lock.T) state =
    let logger = new Logger() 
    match state with
    | Connected client ->
        logger.Info "Lock acquired"
        LockAcquired (client, lockMsg.lock) |> Success
    | _ -> 
        "Invalid state for acquiring lock" |> Failure

let onHelloMessage (helloMsg: Dcpp.Message.Hello.T) state =
    let logger = new Logger() 
    match state with
    | WaitingForAuth (client, lockData, sentNick) ->
        let (Dcpp.Message.Hello.NickData sentNickStr) = sentNick
        let (Dcpp.Message.Hello.NickData receivedNickStr) = helloMsg.nick
        if sentNickStr <> receivedNickStr then
            logger.Error "Sent nick does not equal received nick: `%s`<>`%s`" sentNickStr receivedNickStr
        
        logger.Info "Successfully logged in as %s" receivedNickStr
        LoggedIn (client, lockData, helloMsg.nick) |> Success
    | _ -> 
        "Invalid state for authorization" |> Failure

let onBadPassMessage (msg: Dcpp.Message.BadPass.T) state =
    let logger = new Logger()
    match state with
    | WaitingForAuth (client, lockData, sentNick) ->
        let (Dcpp.Message.Hello.NickData nickStr) = sentNick
        logger.Error "Invalid password for nick %s" nickStr

        // server must close the connection right afterwards, but until then we set our state as ...
        LockAcquired (client, lockData) |> Success
    | _ ->
        "Invalid state for authorization" |> Failure

// action messages
type Action =
| Connect of ConnectionInfo
| SendNick of Dcpp.Message.Hello.NickData
| HandleMessage of Dcpp.Message.T

let handleMessage message state = 
    let logger = new Logger()

    match message with
    | Dcpp.Message.LockT msgData -> onLockMessage msgData state
    | Dcpp.Message.BadPassT msgData -> onBadPassMessage msgData state
    | Dcpp.Message.GetPassT msgData -> 
        logger.Error "GetPass message is not supported yet"
        "GetPass message is not supported yet" |> Failure
    | Dcpp.Message.HelloT msgData -> onHelloMessage msgData state
    | Dcpp.Message.ValidateDeniedT msgData -> 
        logger.Error "ValidateDenied message is not supported yet"
        "ValidateDenied message is not supported yet" |> Failure

let dispatchAction action state =
    let logger = new Logger()

    match action with
    | Connect connectInfo ->
        match connectAsync connectInfo state with
        | Failure s ->
            logger.Fatal "Could not connect to %A" connectInfo
            Failure (sprintf "Could not connect to %A" connectInfo)
        | Success newStateAsync ->
            let newState = newStateAsync |> Async.RunSynchronously
            Success newState
    | SendNick nick ->
        sendNick nick state
    | HandleMessage message ->
        handleMessage message state

let startServer connectInfo nick = 
    let logger = new Logger()

    // TODO make proper cancellation         
    logger.Trace "Creating agent ..."
    let agent = Agent.create NotConnected dispatchAction
    agent.event |> Event.add (fun (state, state') -> 
        logger.Info "Changing state from %A to %A" state state'
    )

    let establishedConnectionEvent = agent.event |> Event.choose (fun (state, state') ->
        match (state, state') with
        | NotConnected, Connected client ->
            (state, state', client) |> Some
        | _ -> 
            None    
    ) 
    // TODO handle clearing of subscription on client.event when status becomes NotConnected
    establishedConnectionEvent |> Event.add (fun (state, state', client) ->
        logger.Info "Established connection"
        let event = client.receivedEvent |> Event.map FDCNet.Util.getString
        event |> Event.add (fun message -> logger.Trace "Got message: %s" message)
        let dcppUnvalidatedEvent = event |> Event.choose (fun message -> 
            match Dcpp.Message.parse message with
            | None -> 
                logger.Warn "Could not parse message: %s" message
                None
            | Some parsedMessage ->
                logger.Trace "Parsed message: %A" parsedMessage
                parsedMessage |> Some
            )
        let dcppEvent = dcppUnvalidatedEvent |> Event.choose (fun message -> 
            match Dcpp.Message.validate message with
            | Failure error -> 
                logger.Error "Received message is not valid: %s" error
                None
            | Success validated ->
                validated |> Some
            )
        dcppEvent |> Event.add (fun msg ->
            agent.post <| HandleMessage msg
        )
    )

    // TODO construct DSL so it looks like `apply Connected LockAquired fun` or something
    agent.event |> Event.filter (
        function
        | Connected _, LockAcquired _ -> true
        | _ -> false    
    ) |> Event.add (fun _ ->
        agent.post <| SendNick nick
    )

    logger.Info "Connecting (%A) ..." connectInfo
    agent.post <| Connect connectInfo
    agent
    // let dispatcher message, state = 
    //     match message with
    //     | Dcpp.Message.LockT msgData -> onLockMessage msgData state
    //     | Dcpp.Message.BadPassT msgData -> onBadPassMessage msgData state
    //     | Dcpp.Message.GetPassT msgData -> 
    //         logger.Error "GetPass message is not supported yet"
    //             state
    //     | Dcpp.Message.HelloT msgData -> onHelloMessage msgData state
    //     | Dcpp.Message.ValidateDeniedT msgData -> 
    //         logger.Error "ValidateDenied message is not supported yet"
    //             state
