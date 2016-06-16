module FDCDomain.MessageQueue

open System

open FDCUtil.Main
open FDCUtil.Main.Regex

open FSharp.Control.Reactive

// errors
type StringError = 
| Missing
| NotASCIIString
| IncludesForbiddenCharacter of char
| MustNotBeShorterThan of int
| CouldntConvert of Exception

type TransportError =
| CouldntConnect of string

type ActionError =
| InvalidState 
| InvalidAction
| DepsAreMissing
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
    let unwrap (ASCIIString s) = s

    let getBytes (ASCIIString s) = getBytes s |> Result.get

module IpAddress =
    type T = IpAddress of string

    let create (s: string) = 
        try
            System.Net.IPAddress.Parse(s) |> Success
        with e ->
            StringError.CouldntConvert e |> Failure 
        |> Result.map (fun ip ->
            ip.ToString() |> IpAddress
        )
    let fold f (IpAddress ip) = f ip
    let unwrap (IpAddress ip) = ip

    let getBytes (IpAddress ip) = getBytes ip |> Result.get

module PositiveInt = 
    type T = PositiveInt of uint64

    type Error = 
    | NegativeValue

    let create (str: string) =
        try
            Convert.ToUInt64(str) |> PositiveInt |> Success
        with ex ->
            StringError.CouldntConvert ex |> Failure

    let fromULong (i: uint64) = PositiveInt i 

    let fold f (PositiveInt i) = f i
    let unwrap (PositiveInt i) = i

module PkData = 
    type T = PkData of string

    let create = mapNullString PkData

    let fold f (PkData s) = f s 

module LockData =
    type T = LockData of ASCIIString.T

    let create (input: string) =
        input
        |> ASCIIString.create
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
    type T = NickData of ASCIIString.T

    let create (str: string) =
        let forbidden_chars = ['|'; '$'; ' ']
        
        let canonicalized = System.Text.RegularExpressions.Regex.Replace(str,"\s"," ").Trim()

        let forbidden = forbidden_chars |> List.tryFind (fun c -> canonicalized.IndexOf(string c) >= 0)
        
        match forbidden with
        | Some c -> StringError.IncludesForbiddenCharacter c |> Failure
        | None ->  
            canonicalized
            |> ASCIIString.create
            |> Result.map NickData

    let fold f (NickData s) = f s
    let unwrap (NickData s) = s

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
    let unwrap (KeyData b) = b

module PasswordData =
    type T = PasswordData of ASCIIString.T

    let create (str: string) =
        let forbidden_chars = ['|'; '$'; ' ']
        
        let canonicalized = System.Text.RegularExpressions.Regex.Replace(str,"\s"," ").Trim()

        let forbidden = forbidden_chars |> List.tryFind (fun c -> canonicalized.IndexOf(string c) >= 0)
        
        match forbidden with
        | Some c -> StringError.IncludesForbiddenCharacter c |> Failure
        | None ->  
            canonicalized
            |> ASCIIString.create
            |> Result.map PasswordData

    let fold f (PasswordData p) = f p
    let unwrap (PasswordData p) = p

    let getBytes (PasswordData pass) = 
        getBytes |> ASCIIString.fold <| pass 
        |> Result.get
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
    let unwrap (PortData p) = p

// domain models
type ConnectionInfo = {
    host: HostnameData.T
    port: PortData.T
}

type ListenInfo = {
    ip: IpAddress.T
    port: PortData.T
}

type LockMessage = {
    lock: LockData.T
    pk: PkData.T
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

type MyInfoMessage = {
    nick: NickData.T
    share_size: PositiveInt.T
}

type NickListMessage = {
    nicks: NickData.T list
}

type OpListMessage = {
    nicks: NickData.T list
}

type QuitMessage = {
    nick: NickData.T
}

type ChatMessageMessage = {
    nick: NickData.T
    message: string
}

type HubTopicMessage = {
    topic: string
}

type HubNameMessage = {
    name: string
}

type SearchMessage = {
    listen_info: ListenInfo
    search_str: string
}

type NickInfo = {
    share_size: PositiveInt.T
}

type NickObj = {
    nick: NickData.T
    nick_info: NickInfo option
    is_Op: bool
}

type ConnectedEnv = {
    connect_info: ConnectionInfo
}

type WaitingForAuthEnv = {
    connect_info: ConnectionInfo
    nick: NickData.T
    key: KeyData.T
}

type WaitingForPassAuthEnv = {
    connect_info: ConnectionInfo
    nick: NickData.T
    password: PasswordData.T
}

type LoggedInEnv = {
    connect_info: ConnectionInfo
    nick: NickData.T
    nicks: NickObj list
}

type SearchAction = {
    listen_info: ListenInfo
    search_str: string
}

// higher-order domain models
type DcppReceiveMessage = 
| Lock of LockMessage
| ValidateDenied 
| GetPass
| BadPass
| Hello of HelloMessage  //TODO change handler for Hello
| LoggedIn
| NickList of NickListMessage
| OpList of OpListMessage
| Quit of QuitMessage
| HubTopic of HubTopicMessage
| HubName of HubNameMessage
| ChatMessage of ChatMessageMessage
| MyInfo of MyInfoMessage
| Ignore_

type DcppSendMessage = 
| ValidateNick of ValidateNickMessage
| MyPass of MyPassMessage
| Version
| MyInfo of MyInfoMessage
| Search of SearchMessage

type AgentAction =
| Connect of ConnectionInfo
// | SendMyInfo
| SendNick of NickData.T * KeyData.T 
| SendPass of PasswordData.T
| RetryNick of NickData.T
| Helloed of NickData.T
| Disconnected
| Disconnect
| NickListed of NickData.T list
| OpListed of NickData.T list
| Quitted of NickData.T
| MyInfoed of NickData.T * NickInfo
| Search of SearchAction

type State = 
| NotConnected
| Connected          of ConnectedEnv
| WaitingForAuth     of WaitingForAuthEnv
| WaitingForPassAuth of WaitingForPassAuthEnv
| LoggedIn           of LoggedInEnv

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
type ITransport = 
    inherit IDisposable 
    abstract Received: IObservable<DcppReceiveMessage>
    abstract Write: DcppSendMessage -> unit
type CreateTransport = ConnectionInfo -> Result<ITransport, TransportError>

type Dependencies = {
    transport: ITransport
}

// infrastructure functions 

let DCNstring_to_DcppMessage input =
    Result.success_workflow_with_string_failures {
        match input with
        | Regex "^\$Hello (.+)\|$" [ nick ] ->
            let! nick_data = NickData.create nick
            return Hello { nick = nick_data }
        | Regex "^\$Quit (.+)\|$" [ nick ] ->
            let! nick_data = NickData.create nick
            return Quit { nick = nick_data }            
        | Regex "^\$BadPass\|$" [] -> 
            return BadPass
        | Regex "^\$GetPass\|$" [] -> 
            return GetPass
        | Regex "^\$ValidateDenide\|$" [] -> 
            return ValidateDenied
        | Regex "^\$Lock (.+) Pk=(.+)\|$" [ lock; pk ] ->
            let! lock_data = LockData.create << DCNtoString <| lock
            let! pk_data = PkData.create pk // TODO check for what fields DCN encoding/decoding should be happening 
            return Lock {
                lock = lock_data
                pk = pk_data 
            }
        | Regex "^\$MyINFO \$ALL (.+?) .*\$(.*?)\$\|$" [ nick_str; share_size_str ] ->
            let! nick_data = NickData.create nick_str
            let! share_size =  
                if share_size_str = String.Empty then
                    PositiveInt.fromULong 0UL |> Success
                else
                    PositiveInt.create share_size_str

            return DcppReceiveMessage.MyInfo {
                nick = nick_data
                share_size = share_size
            }
        | Regex "^\$NickList (.*)\$\$\|$" [ nicklist_str ] ->
            let nicks =
                nicklist_str
                |> String.split "$$"
                |> Seq.map NickData.create
                |> Seq.choose (Result.fold Some (ct None))
                |> List.ofSeq
            
            return NickList {
                nicks = nicks
            }
        | Regex "^\$OpList (.*)\$\$\|$" [ nicklist_str ] ->
            let nicks =
                nicklist_str
                |> String.split "$$"
                |> Seq.map NickData.create
                |> Seq.choose (Result.fold Some (ct None))
                |> List.ofSeq
            
            return OpList {
                nicks = nicks
            }
        | Regex "^\$HubTopic (.+)\|$" [topic] ->
            return HubTopic { 
                topic = topic
            }
        | Regex "^\$HubName (.+)\|$" [hubname] ->
            return HubName { 
                name = hubname
            }            
        | Regex "^\<(.+?)\>\s((\s|.)*)\|$" [nick_str; message; _] ->
            let! nick = NickData.create nick_str
            return ChatMessage {
                nick = nick
                message = message
            }
        | Regex "^\$Search .*\|$" [] ->
            return Ignore_
        | _ -> 
            return! Failure "Couldn't parse"
    }
    |> Result.mapFailure (fun e -> e, input)

let DcppMessage_to_bytes dcpp_message = 
    match dcpp_message with
    | DcppSendMessage.Search s_msg ->
        [
            "$Search " |> getBytes |> Result.get;
            IpAddress.getBytes s_msg.listen_info.ip;
            ":" |> getBytes |> Result.get;
            (PortData.unwrap s_msg.listen_info.port).ToString() |> getBytes |> Result.get;
            " " |> getBytes |> Result.get;
            "F?F?0?1?" |> getBytes |> Result.get;
            s_msg.search_str |> getBytes |> Result.get; // TODO validate search_str
            "$$" |> getBytes |> Result.get;
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.MyPass mp_msg -> 
        [
            "$MyPass " |> getBytes |> Result.get;
            PasswordData.fold ASCIIString.getBytes mp_msg.password;
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat

    | DcppSendMessage.ValidateNick vn_msg ->
        [ 
            "$Key " |> getBytes |> Result.get;
            KeyData.unwrap vn_msg.key;
            "|$ValidateNick " |> getBytes |> Result.get;
            NickData.fold ASCIIString.getBytes vn_msg.nick;
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Version ->
        "$Version 1.0091|" |> getBytes |> Result.get
    | DcppSendMessage.MyInfo mi_msg ->
        [
            "$MyINFO $ALL " |> getBytes |> Result.get;
            NickData.fold ASCIIString.getBytes mi_msg.nick;
            " $ $" |> getBytes |> Result.get;
            [|0x35uy; 0x30uy; 0x01uy|];
            "$$0$" |> getBytes |> Result.get;
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat

// domain logic (functions)
let private validate_state (state, deps) =
    match (state, deps) with
    | NotConnected, _ -> 
        Success ()
    | _, None ->
        // this is a programming error if we get here. should never-ever happen
        Failure InvalidState
    | _ -> 
        Success ()

let private connect (create_transport: CreateTransport) connect_info state =
    match state with
    | NotConnected ->
        create_transport connect_info
        |> Result.mapFailure ActionError.TransportError
        |> Result.mapSuccess (fun transport -> Connected { connect_info = connect_info }, transport)
    | _ -> 
        Failure InvalidAction

let private disconnect deps_maybe = 
    deps_maybe
    |> Option.map (fun deps -> deps.transport.Dispose())
    |> ignore

    (NotConnected, None)

let private add_nonexisting_nick nick' nicks = 
    { nick = nick'; nick_info = None; is_Op = false }::nicks

let private add_nick nick' (nicks: NickObj list) =
    //TODO OMG THAT MUST BE SLOW
    let already_there = nicks |> List.exists (fun nick_obj -> nick' = nick_obj.nick)
    if not already_there then add_nonexisting_nick nick' nicks
    else nicks

let private add_nicks nicks = 
    nicks |> List.fold (fun acc nick -> add_nick nick acc) []

let private remove_nick nick (nicks: NickObj list) =
    //TODO OMG THAT MUST BE SLOW
    List.filter (fun (nick_obj: NickObj) -> nick_obj.nick <> nick) nicks 

let private add_nick_with_info nick nick_info nicks = 
    //TODO OMG THAT MUST BE SLOW
    let exists = List.exists (fun (nick_obj: NickObj) -> nick_obj.nick = nick) nicks
    if exists then
        nicks |> List.map (fun nick_obj -> 
            if nick_obj.nick = nick then { nick_obj with nick_info = nick_info }
            else nick_obj
        )
    else
        {nick = nick; nick_info = nick_info; is_Op = false}::nicks
    
let private mark_nick_as_op nick nicks = 
    let exists = List.exists (fun (nick_obj: NickObj) -> nick_obj.nick = nick) nicks
    if exists then
        nicks |> List.map (fun nick_obj -> 
            if nick_obj.nick = nick then { nick_obj with is_Op = true }
            else nick_obj
        )
    else
        {nick = nick; nick_info = None; is_Op = true}::nicks

let private dispatch_action (create_log: CreateLogger) (create_transport: CreateTransport) action (state, deps_maybe) =
    let log = create_log()
    log.Trace "Dispatching action %A" action

    validate_state (state, deps_maybe) 
    |> Result.collect (fun _ ->
        match action with
        | AgentAction.Search action ->
            // TODO group all sendmessage actions into one group
            deps_maybe
            |> Result.fromOption <| DepsAreMissing
            |> Result.bind <| (fun deps ->
                match state with
                | LoggedIn env ->
                    let msg = DcppSendMessage.Search { listen_info = action.listen_info; search_str = action.search_str }

                    deps.transport.Write msg
                    state |> Success
                | _ -> 
                    Failure InvalidAction
            )
            |> Result.map (fun state' -> state', deps_maybe)
        | AgentAction.SendNick (nick, key) ->
            let msg = ValidateNick { nick = nick; key = key }

            deps_maybe
            |> Result.fromOption <| DepsAreMissing
            |> Result.bind <| (fun deps -> 
                match state with
                | Connected env ->
                    deps.transport.Write msg
                    let state' = WaitingForAuth {
                        connect_info = env.connect_info
                        nick = nick
                        key = key
                    }
                    state' |> Success
                | _ -> 
                    Failure InvalidAction
            ) 
            |> Result.map (fun state' -> state', deps_maybe)
        | AgentAction.SendPass (pass) ->
            let msg = MyPass { password = pass }

            let res = 
                deps_maybe
                |> Result.fromOption <| DepsAreMissing
                |> Result.bind <| (fun deps ->
                    match state with
                    | WaitingForAuth env ->
                        deps.transport.Write msg
                        WaitingForPassAuth {
                            connect_info = env.connect_info
                            nick = env.nick
                            password = pass
                        } |> Success
                    | _ ->
                        Failure InvalidAction
                )
            res
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.Connect ci ->
            connect create_transport ci state
            |> Result.map (fun (state', transport) ->
                let deps' = 
                    match deps_maybe with
                    | None -> { transport = transport }
                    | Some deps -> { deps with transport = transport }
                
                state', Some deps'
            )
        | AgentAction.RetryNick nick' ->
            match state with 
            | WaitingForAuth env ->
                deps_maybe
                |> Result.fromOption <| DepsAreMissing
                |> Result.map (fun deps ->
                    deps.transport.Write << DcppSendMessage.ValidateNick <| {
                        nick = nick'
                        key = env.key
                    } 
                    WaitingForAuth { env with nick = nick' }
                )
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.Helloed nick' ->
            match state with
            | WaitingForAuth { connect_info = ci; nick = nick }
            | WaitingForPassAuth { connect_info = ci; nick = nick } ->
                if nick' <> nick then    
                    // ignoring not "ours" Hello messages 
                    state |> Success
                else
                    let myinfo_msg = DcppSendMessage.MyInfo {nick = nick; share_size = PositiveInt.fromULong 0UL}
                    // we are almost logged in, just need to send $Version and $MyINFO
                    deps_maybe
                    |> Result.fromOption <| DepsAreMissing
                    |>! Result.map (fun deps -> deps.transport.Write (DcppSendMessage.Version))
                    |>! Result.map (fun deps -> deps.transport.Write myinfo_msg)
                    |> Result.map (fun _ -> LoggedIn { connect_info = ci; nick = nick; nicks = []})  
            | LoggedIn env ->
                LoggedIn { env with nicks = (add_nick nick' env.nicks) } |> Success
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.MyInfoed (nick', nick_info') ->
            match state with
            | LoggedIn env ->
                let nicks' = (add_nick_with_info nick' (Some nick_info') env.nicks)
                LoggedIn { env with nicks = nicks' } |> Success
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.Quitted nick' ->
            match state with
            | LoggedIn env ->
                LoggedIn { env with nicks = (remove_nick nick' env.nicks) } |> Success
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.NickListed nicks' ->
            match state with
            | LoggedIn env ->
                LoggedIn { env with nicks = add_nicks nicks' } |> Success
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)
        
        | AgentAction.OpListed nicks ->
            match state with
            | LoggedIn env ->
                let nicks' = nicks |> List.fold (fun nicks' nick -> mark_nick_as_op nick nicks') env.nicks
                LoggedIn { env with nicks = nicks' } |> Success
            | _ -> 
                Failure InvalidAction
            |> Result.map (fun state' -> state', deps_maybe)

        | AgentAction.Disconnect
        | AgentAction.Disconnected ->
            disconnect deps_maybe |> Success
    )
    |> Result.mapFailure (fun e -> e, action, state)
    |>! Result.mapFailure (fun x -> log.Error "Error while dispatching action: %A" x)

let private handle_agent (create_log: CreateLogger) await_terminator connect_info (nick_data, pass_data_maybe) (agent: AgentWithComplexState.T<AgentAction, State*Dependencies option, 'c>) =
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

    // handling received dcpp messages
    full_state_events 
    |> Event.choose (
        function
        | (NotConnected, _), (Connected ci, Some deps) -> (ci, deps) |> Some
        | _ -> None)
    |>! Event.add (fun (ci, deps) -> log.Info "Connected to (%A)" ci) 
    |> Event.add (fun (ci, deps) -> 
        // TODO think about disposing event handling after disconnect ? 
        deps.transport.Received
        |> Control.Observable.scan (fun nick dcpp_msg ->
            log.Trace "Received message %A" dcpp_msg
            match dcpp_msg with
            | Lock msg ->
                agent.post <| SendNick (nick_data, KeyData.create msg.lock) 
                nick
            | ValidateDenied ->
                let nick' = 
                    nick
                    |> NickData.unwrap
                    |> ASCIIString.unwrap
                    |> (+) <| "1"
                    |> NickData.create 
                    |>! Result.mapFailure (fun e -> log.Error "Could not create new nick from old nick %A: %A" nick e)
                    |>! Result.map (fun nick' -> agent.post << RetryNick <| nick') 
                    |> Result.fold id (ct nick)  
                nick'
            | Hello msg ->
                let res = 
                    agent.post_and_reply << Helloed <| msg.nick
                    |> (Result.fold 
                        <| ignore
                        <| (fun e -> 
                            // TODO fix this will trigger even for usual Hellos, not for the one which confirms our login
                            log.Error "Could not finish logging in, disconnecting: %A" e
                            agent.post <| Disconnect
                            ))
                nick
            | DcppReceiveMessage.MyInfo msg ->
                agent.post <| MyInfoed (msg.nick, { share_size = msg.share_size })
                nick
            | Quit msg ->
                agent.post << Quitted <| msg.nick
                nick
            | GetPass ->
                match pass_data_maybe with
                | None -> 
                    // TODO terminate everything somehow ?
                    log.Error "Server asks for password but we don't have any"
                | Some pass_data ->
                    agent.post <| SendPass pass_data
                nick
            | BadPass ->
                log.Error "BadPass for nick %A" nick
                agent.post AgentAction.Disconnected
                nick
            | DcppReceiveMessage.LoggedIn ->
                // WILL it is related to Op users, we dont care about them for a moment
                nick
            | ChatMessage msg ->
                log.Trace "Chat message from %A: %s" msg.nick msg.message
                nick
            | HubTopic msg ->
                log.Info "Hub topic: %s" msg.topic
                nick
            | HubName msg ->
                log.Info "Hub name: %s" msg.name
                nick
            | NickList msg ->
                agent.post <| NickListed msg.nicks
                nick
            | OpList msg ->
                agent.post <| OpListed msg.nicks
                nick
            | Ignore_ ->
                nick
        ) nick_data
        |> Observable.subscribeWithCompletion ignore (fun () -> 
            log.Warn "Disconnected"
            agent.post AgentAction.Disconnected
        )
        |> ignore
    )

    state_changed |> Event.add (
        function
        | (WaitingForAuth _ | WaitingForPassAuth _), LoggedIn env' -> log.Info "Successfully logged in as %A" env'.nick
        | _ -> ()
    )

    // handling reconnection
    state_changed 
    |> Event.filter (
        function
        | _, NotConnected -> true
        | _ -> false)
    |> Event.add (fun _ ->
        log.Info "Disconnected" 
        log.Info "Connecting to (%A)..." connect_info
        agent.post_and_reply <| Connect connect_info
        |> Result.mapFailure (log.Error "Couldnt connect %A")
        |> Result.map (log.Info "Reconnected %A")
        |> ignore
    )

    // connecting to the server
    log.Info "Connecting to (%A)..." connect_info
    let connectResult = agent.post_and_reply <| Connect connect_info

    // waiting for external termination
    match connectResult with
    | Failure e ->
        CouldntConnect e |> Failure
    | Success actionResult -> 
        let res = await_terminator(agent) |> Async.RunSynchronously |> Success

        agent.fetch()
        |> Result.map snd
        |> Result.map (Option.map (fun deps -> deps.transport.Dispose()))
        |> ignore

        res

let start_queue (create_log: CreateLogger) (create_transport: CreateTransport) await_terminator connect_info (nick_data, pass_data_maybe) =
    let log = create_log()
    log.Info "Starting queue..."
    
    let dispatch_action_applied = dispatch_action create_log create_transport
    let handle_agent_applied = handle_agent create_log await_terminator connect_info (nick_data, pass_data_maybe)

    AgentWithComplexState.loop 
    <| (State.NotConnected, None) 
    <| dispatch_action_applied
    <| handle_agent_applied
    