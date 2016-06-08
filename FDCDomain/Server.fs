module FDCDomain.Server

open FDCUtil.Main
open FDCLogger
open FDCNet
open FDCNet.Util

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
let connectAsync connectinfo state =
    let logger = new Logger() 
    match state with
    | NotConnected ->
        async {
            logger.Info "Connecting (%s:%i) ..." connectinfo.host connectinfo.port
            let! client = Dcpp.startClientAsync connectinfo.host connectinfo.port
            logger.Info "Connected %s:%i" connectinfo.host connectinfo.port
            
            return Connected client
        } |> Success
    | _ -> 
        "Already connected or connecting" |> Failure

let lockAcquired (lockMsg: Dcpp.Message.Lock.T) state =
    let logger = new Logger() 
    match state with
    | Connected client ->
        logger.Info "Lock acquired"
        LockAcquired (client, lockMsg.lock) |> Success
    | _ -> 
        "Invalid state for acquiring lock" |> Failure

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
            
let loggedIn (helloMsg: Dcpp.Message.Hello.T) state =
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
