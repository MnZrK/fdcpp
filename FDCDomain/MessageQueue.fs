module FDCDomain.MessageQueue

open System

open FDCUtil.Main

// infrastructure interfaces
type ILogger =
    abstract Trace: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract TraceException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract Debug: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract DebugException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract Info: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract InfoException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract Warn: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract WarnException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract Error: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract ErrorException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract Fatal: fmt: Printf.StringFormat<'a, unit> -> 'a
    abstract FatalException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
type CreateLogger = unit -> ILogger 

// errors
type StringError = 
| Missing
| NotASCIIString
| MustNotBeShorterThan of int
| CouldntConvert of Exception

type TransportError =
| CouldntConnect

type ActionError =
| InvalidState of string
| InvalidAction of string
| TransportError of TransportError

type QueueError<'a> = 
| CouldntConnect of 'a

// utilities
let getBytes (str: string) = 
    try System.Text.Encoding.ASCII.GetBytes(str) |> Success
    with ex -> CouldntConvert ex |> Failure
        
let getByte (c: char) = 
    try System.Convert.ToByte(c) |> Success
    with ex -> CouldntConvert ex |> Failure
    
let getString bytes = 
    try System.Text.Encoding.ASCII.GetString(bytes) |> Success
    with ex -> CouldntConvert ex |> Failure

let byteToDCN b = 
    let res = 
        match b with
        | 0uy -> getBytes "/%DCN000%/"
        | 5uy -> getBytes "/%DCN005%/"
        | 36uy -> getBytes "/%DCN036%/"
        | 96uy -> getBytes "/%DCN096%/"
        | 124uy -> getBytes "/%DCN124%/"
        | 126uy -> getBytes "/%DCN126%/"
        | b -> [|b|] |> Success

    match res with 
    | Success bytes -> bytes
    | _ -> failwith "It is impossible to get here, function always succeeds"

let stringToDCN (s: string) = 
    let getBytesL = getBytes >> Result.map List.ofArray 
    let byteToDCNL = byteToDCN >> List.ofArray |> List.collect
    let getStringL = List.toArray >> getString

    s
    |> getBytesL
    |> Result.map byteToDCNL
    |> Result.bind <| getStringL

let DCNtoString (str: string) = 
    let str0uy = getString [|0uy|] |> Result.get 
    let str5uy = getString [|5uy|] |> Result.get
    let str36uy = getString [|36uy|] |> Result.get
    let str96uy = getString [|96uy|] |> Result.get
    let str124uy = getString [|124uy|] |> Result.get
    let str126uy = getString [|126uy|] |> Result.get

    let matches = [
        ("/%DCN000%/", str0uy);
        ("/%DCN005%/", str5uy);
        ("/%DCN036%/", str36uy);
        ("/%DCN096%/", str96uy);
        ("/%DCN124%/", str124uy);
        ("/%DCN126%/", str126uy)            
    ]

    matches
    |> List.fold (fun (res: string) (strFrom, strTo) ->
        res.Replace(strFrom, strTo)
    ) str

let mapNullString f (s: string) = 
    match s with
    | null -> StringError.Missing |> Failure
    | _ -> f s |> Success

// primitive types
module ASCIIString = 
    type T = ASCIIString of string

    let create =
        function
        | null -> StringError.Missing |> Failure
        | s -> 
            let isAscii = 
                String.forall 
                <| (fun c -> 
                        match getByte c with 
                        | Success b ->
                            126uy >= b && b >= 32uy
                        | _ -> 
                            false
                    ) 
                <| s 
            if isAscii then
                ASCIIString s |> Success
            else
                Failure StringError.NotASCIIString

    let fold f (ASCIIString s) = f s

    let getBytes (ASCIIString s) = getBytes s |> Result.get

module Pk = 
    type T = Pk of string

    let create = mapNullString Pk

    let fold f (Pk s) = f s 

module LockData =
    type T = LockData of ASCIIString.T

    let create (input: string) =
        ASCIIString.create input
        |> Result.bind <| 
        (fun s ->
            let length = ASCIIString.fold String.length s
            if length < 2 then
                StringError.MustNotBeShorterThan 2 |> Failure
            else 
                LockData s |> Success
        )

    let fold f (LockData s) = f s

module NickData = 
    type T = NickData of string

    let create = mapNullString NickData

    let fold f (NickData s) = f s

module KeyData = 
    type T = KeyData of byte[]

    let create (lockData: LockData.T) =
        let lockBytes = LockData.fold ASCIIString.getBytes lockData
        let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy) 

        let lockLen = lockBytes.Length
        let key = Array.init lockLen (fun index -> 
            if (index = 0) then
                lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
            else
                lockBytes.[index] ^^^ lockBytes.[index-1]
        ) 
        
        key |> Array.map nibbleSwap |> Array.collect byteToDCN |> KeyData

    let fold f (KeyData b) = f b

module PasswordData =
    type T = PasswordData of string

    let create = mapNullString PasswordData

    let fold f (PasswordData p) = f p

module HostnameData =
    type T = HostnameData of string

    let create = mapNullString HostnameData

    let fold f (HostnameData h) = f h

module PortData = 
    type T = PortData of int

    type Error = 
    | Negative
    | TooBig

    let create port =
        match port with
        | _ when port <= 0 -> Error.Negative |> Failure
        | _ when port >= 65535 -> Error.TooBig |> Failure
        | _ -> PortData port |> Success 
    
    let fold f (PortData p) = f p

// domain models
type LockMessage = {
    lock: LockData.T
    pk: Pk.T
}

type HelloMessage = {
    nick: NickData.T
}

type ValidateNickMessage = {
    key: KeyData.T
    nick: NickData.T
}

type MyPassMessage = {
    password: PasswordData.T
}

type ConnectionInfo = {
    host: HostnameData.T
    port: PortData.T
}

// higher-order domain models
type DcppReceiveMessage = 
    | Lock of LockMessage
    | ValidateDenied 
    | GetPass
    | BadPass
    | Hello of HelloMessage
    | LoggedIn

type DcppSendMessage = 
    | ValidateNick of ValidateNickMessage
    | MyPass of MyPassMessage

type AgentAction =
    | SendMessage of DcppSendMessage
    | ReceiveMessage of DcppReceiveMessage
    | Connect of ConnectionInfo

type State = 
| NotConnected
| Connected      of ConnectionInfo
| LockAcquired   of ConnectionInfo * LockData.T 
| WaitingForAuth of ConnectionInfo * LockData.T * NickData.T
| LoggedIn       of ConnectionInfo * LockData.T * NickData.T

// dont know how to name this section
type ITransport = 
    inherit IDisposable 
    abstract Received: IEvent<DcppReceiveMessage>
    abstract Write: DcppSendMessage -> unit
type CreateTransport = ConnectionInfo -> Result<ITransport, TransportError>

type Dependencies = {
    transport: ITransport
}

// domain logic (functions)
let internal validate_state (state, deps) =
    match (state, deps) with
    | NotConnected, _ -> 
        Success ()
    | _, None -> 
        ActionError.InvalidState "We are connected but no connection-related functions are available" 
        |> Failure
    | _ -> 
        Success ()

let internal dispatch_action (create_log: CreateLogger) (create_transport: CreateTransport) action (state, deps_maybe) =
    let log = create_log()
    log.Trace "Dispatching action %A" action

    validate_state (state, deps_maybe) 
    |> Result.collect (fun _ ->
        match action with
        | SendMessage msg ->
            match deps_maybe with
            | None -> 
                ActionError.InvalidAction "Trying to send message with no connect-related functions available" 
                |> Failure
            | Some deps ->
                match msg with
                | ValidateNick vn_msg ->
                    match state with
                    | LockAcquired (ci, lock_data) ->
                        deps.transport.Write msg 
                        (WaitingForAuth (ci, lock_data, vn_msg.nick), deps_maybe) |> Success  
                    | _ -> 
                        ActionError.InvalidAction "Trying to send nick when state is not LockAcquired"
                        |> Failure
                | MyPass mp_msg ->
                    match state with
                    | WaitingForAuth _ -> 
                        deps.transport.Write msg
                        (state, deps_maybe) |> Success
                    | _ -> 
                        ActionError.InvalidAction "Trying to send pass when state is not WaitingForAuth"
                        |> Failure
        | Connect ci ->
            match state with
            | NotConnected ->
                let transport_result = create_transport ci
                match transport_result with
                | Failure e -> 
                    ActionError.TransportError e |> Failure
                | Success transport ->
                    let deps' =
                        match deps_maybe with
                        | None -> { transport = transport }
                        | Some deps -> { deps with transport = transport }

                    (Connected ci, Some deps') |> Success
            | _ -> 
                ActionError.InvalidAction "Trying to connect when already connected" 
                |> Failure
        | ReceiveMessage msg ->
            match msg with
            | Lock l_msg ->
                match state with
                | Connected ci ->
                    (LockAcquired (ci, l_msg.lock), deps_maybe) |> Success
                | _ -> 
                    ActionError.InvalidAction "Received lock when state is not `Connected`" 
                    |> Failure
            | Hello h_msg ->
                match state with
                | WaitingForAuth (ci, lock, nick) ->
                    (LoggedIn (ci, lock, nick), deps_maybe) |> Success
                | _ ->
                    ActionError.InvalidAction "Received hello when state is not `WaitingForAuth`" 
                    |> Failure

    )
    |>! Result.mapFailure (fun x -> log.Error "Error while dispatching action: %A" x)

let internal handle_agent (create_log: CreateLogger) await_terminator connect_info (nick_data, pass_data) (agent: AgentWithComplexState.T<AgentAction, State*Dependencies option, 'c>) =
    let log = create_log()

    log.Trace "We are inside agent now!"

    // data transformation for convenience
    let full_state_events = agent.state_changed
    let state_changed = 
        agent.state_changed 
        |> Event.map (fun ((state, _), (state', _)) ->
            (state, state')
        )
        |>! Event.add (fun (state, state') -> log.Trace "State changed from %A to %A" state state')

    // translating messages received from server to the agent
    full_state_events 
    |> Event.choose (
        function
        | (NotConnected, _), (Connected ci, Some deps) -> (ci, deps) |> Some
        | _ -> None)
    |>! Event.add (fun (ci, deps) -> log.Info "Connected to (%A)" ci) 
    |> Event.add (fun (ci, deps) -> 
        // TODO think about disposing event handling after reconnect ? 
        deps.transport.Received
        |>! Event.add (fun dcpp_msg -> log.Trace "Received message %A" dcpp_msg)
        |> Event.add (fun dcpp_msg -> agent.post << ReceiveMessage <| dcpp_msg)
    )

    // when Connected -> LockAcquired : send nick
    state_changed 
    |> Event.choose (
        function
        | Connected _, LockAcquired (_, lock_data) -> Some lock_data
        | _ -> None
    )
    |>! Event.add (fun lock_data -> log.Info "Acquired lock %A" lock_data)
    |> Event.add (fun lock_data -> 
        let send_nick_message = 
            SendMessage <| ValidateNick {
                key = KeyData.create lock_data
                nick = nick_data
            }  
        agent.post send_nick_message
    )

    // connecting to the server
    log.Info "Connecting to (%A)..." connect_info
    let connectResult = agent.post_and_reply <| Connect connect_info

    // waiting for external termination
    match connectResult with
    | Failure e ->
        CouldntConnect e |> Failure
    | Success actionResult -> 
        await_terminator(agent) |> Async.RunSynchronously |> Success

let start_queue (create_log: CreateLogger) (create_transport: CreateTransport) await_terminator connect_info (nick_data, pass_dat) =
    let log = create_log()
    log.Info "Starting queue..."
    
    let dispatch_action_applied = dispatch_action create_log create_transport
    let handle_agent_applied = handle_agent create_log await_terminator connect_info (nick_data, pass_dat)

    AgentWithComplexState.loop 
    <| (State.NotConnected, None) 
    <| dispatch_action_applied
    <| handle_agent_applied
    