[<AutoOpen>]
module FDCDomain.Presentation

open System

open FDCUtil

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

// it is fine for parsing function to be long
[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "SourceLength", CheckId = "MaxLinesInFunction")>] 
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
        | Regex "^\$Supports .*\|$" [] ->
            return DcppReceiveMessage.Supports
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
        | Regex "^\$SR (\w+) (.+?).\d+ \d+/\d+.TTH:(.+) \(.+?\)\|$" [ nick_str; path_str; tth_str ] ->
            let! nick_data = NickData.create nick_str
            
            return SR {
                SRMessage.filepath = path_str
                SRMessage.nick_who_has_file = nick_data
                SRMessage.tth = tth_str
            }
        // TODO add parsing of Key messages (note that they are not strings, they are bytes)
        | _ ->
            return! Failure "Couldn't parse"
    }
    |> Result.mapFailure (fun e -> e, input)

// it is fine for parsing function to be long
[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "SourceLength", CheckId = "MaxLinesInFunction")>] 
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
    | DcppSendMessage.Supports ->
        "$Supports ADCGet|" |> getBytes |> Result.get
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
        let res =
            [
                "$ADCGET file TTH/" |> getBytes |> Result.get
                g_msg.tth |> getBytes |> Result.get
                " 0 1048576 ZL1|" |> getBytes |> Result.get
            ]
            |> Array.concat
        printfn "sending get %A" (getString res)
        res

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
