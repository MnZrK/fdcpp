module FDCDomain.MessageQueue

open System
open System.Threading

open FDCUtil

open FSharp.Control.Reactive
open FSharpx.Control

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

let bytesToDCN = Array.collect byteToDCN

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

    let generate len =
        let rnd = System.Random()
        let res = 
            Array.init len (fun _ -> byte (rnd.Next(32, 127)))
            |> getString 
            |> Result.get
            |> ASCIIString
        res

    let apply f (ASCIIString s) = f s
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
    let apply f (IpAddress ip) = f ip
    let unwrap (IpAddress ip) = ip

    let getBytes (IpAddress ip) = getBytes ip |> Result.get

module PositiveInt =
    /// 0 is inclusive
    type T = PositiveInt of uint64

    type Error =
    | NegativeValue

    let create (str: string) =
        try
            Convert.ToUInt64(str) |> PositiveInt |> Success
        with ex ->
            StringError.CouldntConvert ex |> Failure

    let fromULong (i: uint64) = PositiveInt i

    let apply f (PositiveInt i) = f i
    let unwrap (PositiveInt i) = i

// domain primitive types
module PkData =
    type T = PkData of string

    let create = mapNullString PkData

    let apply f (PkData s) = f s

module LockData =
    type T = LockData of ASCIIString.T

    let create (input: string) =
        input
        |> ASCIIString.create
        |> Result.bind <|
        (fun s ->
            let length = ASCIIString.apply String.length s
            if length < 2 then
                StringError.MustNotBeShorterThan 2 |> Failure
            else
                LockData s |> Success
        )
    let generate () =
        let rnd = System.Random()
        let len = rnd.Next(81, 134)
        ASCIIString.generate len |> LockData

    let apply f (LockData s) = f s
    let unwrap (LockData s) = s

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

    let apply f (NickData s) = f s
    let unwrap (NickData s) = s

module KeyData =
    type T = KeyData of byte[]

    let create (lockData: LockData.T) =
        let lockBytes = LockData.apply ASCIIString.getBytes lockData
        let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy)

        let lockLen = lockBytes.Length
        let key = Array.init lockLen (fun index ->
            if (index = 0) then
                lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
            else
                lockBytes.[index] ^^^ lockBytes.[index-1]
        )

        key |> Array.map nibbleSwap |> Array.collect byteToDCN |> KeyData

    let apply f (KeyData b) = f b
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

    let apply f (PasswordData p) = f p
    let unwrap (PasswordData p) = p

    let getBytes (PasswordData pass) =
        getBytes |> ASCIIString.apply <| pass
        |> Result.get
module HostnameData =
    type T = HostnameData of string

    let create = mapNullString HostnameData

    let apply f (HostnameData h) = f h

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

    let apply f (PortData p) = f p
    let unwrap (PortData p) = p

module Direction = 
    type T =
    | Upload
    | Download

    let create str = 
        match str with
        | "Upload" -> Success Upload
        | "Download" -> Success Download
        | _ -> StringError.CouldntConvert (exn "doesn't match") |> Failure

// infrastructure types & interfaces
type ConnectionInfo =
    { host: HostnameData.T
    ; port: PortData.T }

type ListenInfo =
    { ip: IpAddress.T
    ; port: PortData.T }

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

// domain models
type UserInfo = { share_size: PositiveInt.T }

type User =
    { nick: NickData.T
    ; user_info: UserInfo option
    ; is_Op: bool }

type UserMapType = MapWithArbKeyType<NickData.T, User>
// domain models (for Dcpp messages)
type LockMessage =
    { lock: LockData.T
    ; pk: PkData.T }

type HelloMessage = { nick: NickData.T }

type ValidateNickMessage =
    { key: KeyData.T
    ; nick: NickData.T }

type MyPassMessage = { password: PasswordData.T }

type MyInfoMessage =
    { nick: NickData.T
    ; share_size: PositiveInt.T }

type NickListMessage = { nicks: NickData.T list }

type OpListMessage = { nicks: NickData.T list }

type QuitMessage = { nick: NickData.T }

type ChatMessageMessage =
    { nick: NickData.T
    ; message: string }

type HubTopicMessage = { topic: string }

type HubNameMessage = { name: string }

type SearchMessage =
    { listen_info: ListenInfo
    ; search_str: string }

type ConnectToMeMessage = 
    { listen_info: ListenInfo
    ; remote_nick: NickData.T }

type MyNickMessage = { nick: NickData.T }

type GetMessage = 
    { filepath: string
    /// 1 = beginning of file
    ; start_at_byte: int64 }

type DirectionMessage =
    { direction: Direction.T
    ; priority: int }

type KeyMessage = { key: KeyData.T }

type FileLengthMessage = { file_length: PositiveInt.T }

// domain models (for State environments)
type ConnectedEnv = { connect_info: ConnectionInfo }

type WaitingForAuthEnv =
    { connect_info: ConnectionInfo
    ; nick: NickData.T
    ; key: KeyData.T }

type WaitingForPassAuthEnv =
    { connect_info: ConnectionInfo
    ; nick: NickData.T
    ; password: PasswordData.T }

type LoggedInEnv =
    { connect_info: ConnectionInfo
    ; nick: NickData.T
    ; users: UserMapType }

// domain models (for Action environments)
type SearchAction =
    { listen_info: ListenInfo
    ; search_str: string }

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
| IgnoreIt
| MyNick of MyNickMessage
| Direction of DirectionMessage
| Key of KeyMessage
| FileLength of FileLengthMessage

type DcppSendMessage =
| ValidateNick of ValidateNickMessage
| MyPass of MyPassMessage
| Version
| MyInfo of MyInfoMessage
| Search of SearchMessage
| ConnectToMe of ConnectToMeMessage
| MyNick of MyNickMessage
| Get of GetMessage
| Lock of LockMessage
| Send
| Direction of DirectionMessage
| Key of KeyMessage

type SendAction =
| SendNick of NickData.T * KeyData.T
| SendPass of PasswordData.T
| RetryNick of NickData.T
| Search of SearchAction

/// Actions which are available only for LoggedIn state
type MainAction = 
| NickListed of NickData.T list
| OpListed of NickData.T list
| Quitted of NickData.T
| MyInfoed of NickData.T * UserInfo

type AgentAction =
| Send of SendAction
| Main of MainAction
| Helloed of NickData.T
| Connect of ConnectionInfo
| Disconnected
| Disconnect

type State =
| NotConnected
| Connected          of ConnectedEnv
| WaitingForAuth     of WaitingForAuthEnv
| WaitingForPassAuth of WaitingForPassAuthEnv
| LoggedIn           of LoggedInEnv

type ReceivedHandlerEnv = { nick: NickData.T }

// presentation interfaces
type ITransport =
    inherit IDisposable
    abstract Received: IObservable<DcppReceiveMessage>
    abstract Write: DcppSendMessage -> unit
type CreateTransport = ConnectionInfo -> Result<ITransport, TransportError>

type Dependencies = {
    transport: ITransport
}

type Agent = AgentWithComplexState.T<AgentAction, State*Dependencies option, (ActionError*AgentAction*State)>

// presentation functions
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
            return DcppReceiveMessage.Lock {
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
                |> Seq.choose (Result.fork Some (ct None))
                |> List.ofSeq

            return NickList {
                nicks = nicks
            }
        | Regex "^\$OpList (.*)\$\$\|$" [ nicklist_str ] ->
            let nicks =
                nicklist_str
                |> String.split "$$"
                |> Seq.map NickData.create
                |> Seq.choose (Result.fork Some (ct None))
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
            return IgnoreIt
        | Regex "^\$MyNick (.+)\|$" [nick_str] ->
            let! nick_data = NickData.create nick_str
            return DcppReceiveMessage.MyNick {
                nick = nick_data
            }
        | Regex "^\$Direction (Download|Upload) (\d+)\|$" [direction; priority] ->
            
            let! direction_data = Direction.create direction
            let! priority_data = 
                try Convert.ToInt32 priority |> Success
                with ex -> Failure ex

            return DcppReceiveMessage.Direction {
                DirectionMessage.direction = direction_data
                DirectionMessage.priority = priority_data
            }
        | Regex "^\$FileLength (.+)\|$" [ length_str ] ->
            let! file_length = PositiveInt.create length_str
            
            return FileLength {
                file_length = file_length
            }
        // TODO add parsing of Key messages (note that they are not strings, they are bytes)
        | _ ->
            return! Failure "Couldn't parse"
    }
    |> Result.mapFailure (fun e -> e, input)

let DcppMessage_to_bytes dcpp_message =
    match dcpp_message with
    | DcppSendMessage.Search s_msg ->
        [
            "$Search " |> getBytes |> Result.get
            IpAddress.getBytes s_msg.listen_info.ip
            ":" |> getBytes |> Result.get
            (PortData.unwrap s_msg.listen_info.port).ToString() |> getBytes |> Result.get
            " " |> getBytes |> Result.get
            "F?F?0?1?" |> getBytes |> Result.get
            s_msg.search_str |> getBytes |> Result.get // TODO validate search_str
            "$$" |> getBytes |> Result.get
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.MyPass mp_msg ->
        [
            "$MyPass " |> getBytes |> Result.get
            PasswordData.apply ASCIIString.getBytes mp_msg.password
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat

    | DcppSendMessage.ValidateNick vn_msg ->
        [
            // TODO extract key into separate message
            "$Key " |> getBytes |> Result.get
            KeyData.unwrap vn_msg.key
            "|$ValidateNick " |> getBytes |> Result.get
            NickData.apply ASCIIString.getBytes vn_msg.nick
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Key k_msg ->
        [
            "$Key " |> getBytes |> Result.get
            KeyData.unwrap k_msg.key
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Version ->
        "$Version 1.0091|" |> getBytes |> Result.get
    | DcppSendMessage.MyInfo mi_msg ->
        [
            "$MyINFO $ALL " |> getBytes |> Result.get
            NickData.apply ASCIIString.getBytes mi_msg.nick
            " $ $" |> getBytes |> Result.get
            [|0x35uy; 0x30uy; 0x01uy|]
            "$$0$" |> getBytes |> Result.get
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.ConnectToMe cm_msg ->
        [
            "$ConnectToMe " |> getBytes |> Result.get
            NickData.apply ASCIIString.getBytes cm_msg.remote_nick
            " " |> getBytes |> Result.get
            IpAddress.getBytes cm_msg.listen_info.ip
            ":" |> getBytes |> Result.get
            (PortData.unwrap cm_msg.listen_info.port).ToString() |> getBytes |> Result.get
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.MyNick mn_msg ->
        [
            "$MyNick " |> getBytes |> Result.get
            NickData.apply ASCIIString.getBytes mn_msg.nick
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    // TODO check all DCN related stuff and make it secure and whatnot
    | DcppSendMessage.Get g_msg ->
        [
            "$Get " |> getBytes |> Result.get
            g_msg.filepath |> getBytes |> Result.get
            "$" |> getBytes |> Result.get
            g_msg.start_at_byte.ToString() |> getBytes |> Result.get
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Lock l_msg ->
        [
            "$Lock " |> getBytes |> Result.get
            LockData.apply ASCIIString.getBytes l_msg.lock |> bytesToDCN
            " Pk=" |> getBytes |> Result.get
            PkData.apply getBytes l_msg.pk |> Result.get 
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Direction d_msg ->
        let direction_str =
            match d_msg.direction with
            | Direction.Upload -> "Upload"
            | Direction.Download -> "Download"
        let priority_str = d_msg.priority.ToString() 

        [
            "$Direction " |> getBytes |> Result.get
            direction_str |> getBytes |> Result.get
            " " |> getBytes |> Result.get
            priority_str |> getBytes |> Result.get
            "|" |> getBytes |> Result.get
        ]
        |> Array.concat
    | DcppSendMessage.Send ->
        "$Send|" |> getBytes |> Result.get

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
        deps.transport.Write Version
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
    | DcppReceiveMessage.FileLength _ 
    | DcppReceiveMessage.Key _
    | DcppReceiveMessage.MyNick _
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
