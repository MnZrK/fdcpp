module FDCConcoleUI

open System
open System.Threading
open FDCLogger
open FDCNet

let getBytes (str: string) = System.Text.Encoding.ASCII.GetBytes(str)

[<EntryPoint>]
let main argv =
    // let host = "p2p.academ.org"
    let host = "localhost"
    let port = 411

    let logger = new Logger() 
    logger.Info "FDCConsoleUI is starting! args: %A" argv
    
    logger.Info "Starting Tcp client..."
    let client = Dcpp.startClient host port

    try
        let rawEvent = client.receivedEvent
        let event = rawEvent |> Event.map System.Text.Encoding.ASCII.GetString

        let readMessageAsync () = Async.AwaitEvent(event) 
        
        let rawWriteMsg = client.write 
        let writeMessage (msg: string) = 
            logger.Trace "Sending message...: %s" msg
            rawWriteMsg (System.Text.Encoding.ASCII.GetBytes(msg)) 
            logger.Trace "Message has been sent"
                
        event |> Event.add (fun message -> logger.Trace "Got message: %s" message)
        
        let dcppUnvalidatedEvent = event |> Event.choose (fun message -> 
            match Dcpp.parseMessage message with
            | None -> 
                logger.Warn "Could not parse message: %s" message
                None
            | Some parsedMessage ->
                logger.Trace "Parsed message: %A" parsedMessage
                parsedMessage |> Some
            )
        let dcppEvent = dcppUnvalidatedEvent |> Event.choose (fun message -> 
            match Dcpp.validateMessage message with
            | Dcpp.Fail error -> 
                logger.Error "Received message is not validated: %s" error
                None
            | Dcpp.Success ->
                Some message
            )
            
        let readDcppMessageAsync () = Async.AwaitEvent(dcppEvent)
        
        async {
            // lock message
            let! dcppMsg = readDcppMessageAsync()
                       
            let lock = 
                match dcppMsg with
                | Dcpp.LockMessage lock ->
                    logger.Info "Got lock %s" lock.lock
                    logger.Info "Got pk %s" lock.pk
                    lock
                | _ -> failwith "Could not parse lock message"

            // authorization
            let nick = "MnZrKk"
            let authRequest = Array.concat [getBytes "$Key "; Dcpp.convertLockToKey <| getBytes lock.lock; getBytes ("|$ValidateNick "+nick+"|") ]
            rawWriteMsg authRequest
            
            let! authResponse = readDcppMessageAsync()
            
            let authMsg =
                match authResponse with
                | Dcpp.AuthMessage authMsg ->
                    logger.Info "Got auth message %A" authMsg
                    authMsg
                | _ -> failwith "Could not parse auth message"
 
            match authMsg with
            | Dcpp.BadPass -> failwith "Bad password"
            | Dcpp.GetPass -> failwith "Password is not supported"
            | Dcpp.ValidateDenied -> failwith "Could not login, validate denied"
            | Dcpp.Hello n ->
                if n <> nick then
                    logger.Warn "Nick returned from server does not match our actual nick"
                logger.Info "Logged in"
 
            ()
        } |> Async.RunSynchronously
        
        Thread.Sleep(10000)
    finally
        client.dispose()

    logger.Info "Shutting down..."
        
    0 // return an integer exit code
