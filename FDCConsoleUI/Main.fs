module FDCConcoleUI

open System
open System.Threading
open FDCLogger
open FDCNet

let getBytes (str: string) = System.Text.Encoding.ASCII.GetBytes(str)

[<EntryPoint>]
let main argv =
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
            let! dcppMsg = readDcppMessageAsync()
                       
            let hello = 
                match dcppMsg with
                | Dcpp.LockMessage hello ->
                    logger.Info "got lock %s" hello.lock
                    logger.Info "got pk %s" hello.pk
                    hello
                | _ -> failwith "Could not parse lock message"
            
            let response = Array.concat [getBytes "$Key "; Dcpp.convertLockToKey <| getBytes hello.lock; getBytes "|$ValidateNick MnZrKk|" ]
             
            rawWriteMsg response
        } |> Async.RunSynchronously
        
        Thread.Sleep(10000)
    finally
        client.dispose()

    logger.Info "Shutting down..."
        
    0 // return an integer exit code
