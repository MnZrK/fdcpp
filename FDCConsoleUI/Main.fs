module FDCConcoleUI

open System
open System.Threading

open FDCUtil.Main
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
            
        let readDcppMessageAsync () = Async.AwaitEvent(dcppEvent)
        
        async {
            // lock message
            let! lockResponse = readDcppMessageAsync()
                       
            let lockMsg = 
                match lockResponse with
                | Dcpp.Message.LockT lockMsg ->
                    logger.Info "Got lock %A" lockMsg.lock
                    logger.Info "Got pk %s" lockMsg.pk
                    lockMsg
                | _ -> failwith "Could not parse lock message"

            // authorization
            let nick = "MnZrKk"
            let authRequest = Array.concat [getBytes "$Key "; Dcpp.Message.Lock.calculateKey lockMsg.lock; getBytes ("|$ValidateNick "+nick+"|") ]
            rawWriteMsg authRequest
            
            let! authResponse = readDcppMessageAsync()
            
            match authResponse with
            | Dcpp.Message.BadPassT _ -> failwith "Bad password"
            | Dcpp.Message.GetPassT _ -> failwith "Password is not supported"
            | Dcpp.Message.ValidateDeniedT _ -> failwith "Could not login, validate denied"
            | Dcpp.Message.HelloT msg ->
                if msg.nick <> Dcpp.Message.Hello.NickData nick then
                    logger.Warn "Nick returned from server does not match our actual nick"
                logger.Info "Logged in"
            | _ -> failwith "Expected auth response" 
                
        } |> Async.RunSynchronously
        
        Thread.Sleep(10000)
    finally
        client.dispose()

    logger.Info "Shutting down..."
        
    0 // return an integer exit code
