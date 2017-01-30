[<AutoOpen>]
module FDCDomain.Models

open System
open System.Threading

open FSharp.Control.Reactive
open FSharpx.Control

open FDCUtil

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
    { tth: string }

type DirectionMessage =
    { direction: Direction.T
    ; priority: int }

type KeyMessage = { key: KeyData.T }

type SRMessage = 
    { nick_who_has_file: NickData.T
    ; filepath: string
    ; tth: string }

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

type ConnectToMeAction =
    { listen_info: ListenInfo
    ; remote_nick: NickData.T }

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
| SR of SRMessage
| Supports

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
| Direction of DirectionMessage
| Key of KeyMessage
| Supports

type SendAction =
| SendNick of NickData.T * KeyData.T
| SendPass of PasswordData.T
| RetryNick of NickData.T
| Search of SearchAction
| ConnectToMe of ConnectToMeAction
| SendSupports 

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
