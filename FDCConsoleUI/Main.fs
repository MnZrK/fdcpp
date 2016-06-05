module FDCConcoleUI

open System
open System.Threading
open FDCLogger
open FDCNet

[<EntryPoint>]
let main argv =
    let logger = new Logger() 
    logger.Info "FDCConsoleUI is starting! args: %A" argv
    
    logger.Info "Starting Tcp client..."
    let disposable, rawEvent, rawPost = Tcp.startClient "localhost" 411
    let event = rawEvent |> Event.map System.Text.Encoding.ASCII.GetString 
    let post (msg: string) = 
        logger.Trace "Sending message...: %s" msg
        rawPost (System.Text.Encoding.ASCII.GetBytes(msg)) 
        logger.Trace "Message has been sent"

    try
        event |> Event.add (fun message -> logger.Trace "got message: %s" message)
        
        Async.RunSynchronously(async {
            let! message = Async.AwaitEvent(event)
            logger.Info "got message: %s" message
            let response = "24:53:75:70:70:6f:72:74:73:20:55:73:65:72:43:6f:6d:6d:61:6e:64:20:4e:6f:47:65:74:49:4e:46:4f:20:4e:6f:48:65:6c:6c:6f:20:55:73:65:72:49:50:32:20:54:54:48:53:65:61:72:63:68:20:5a:50:69:70:65:30:20:47:65:74:5a:42:6c:6f:63:6b:20:7c:24:4b:65:79:20:11:d1:c0:11:b0:a0:10:10:41:20:d1:b1:b1:c0:c0:30:f1:13:53:d0:e6:d6:70:b0:d0:a2:10:93:80:02:a0:c0:95:44:10:d1:33:c1:62:b2:32:2f:25:44:43:4e:30:30:30:25:2f:c5:f4:01:15:7c:24:56:61:6c:69:64:61:74:65:4e:69:63:6b:20:4d:6e:5a:72:4b:6b:7c"
            let replyMessage = response.Split [|':'|] |> Array.map (fun x -> Byte.Parse(x,System.Globalization.NumberStyles.AllowHexSpecifier)) 
            rawPost replyMessage
        })
        Thread.Sleep(10000)
    finally 
        disposable.Dispose()
    
    logger.Info "Shutting down..."
        
    0 // return an integer exit code
