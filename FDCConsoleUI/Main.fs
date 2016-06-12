module FDCConcoleUI

open System
open System.Threading

open FDCUtil.Main
open FDCLogger
open FDCNet
open FDCDomain.MessageQueue

[<EntryPoint>]
let main argv =
    let logger = new Logger ()
    logger.Info "FDCConsoleUI is starting! args: %A" argv
    
    let connect_info = {
        host = HostnameData.create "localhost" |> Result.get
        port = PortData.create 411 |> Result.get
    }
    let create_log () = (new Logger() :> ILogger)  

    let res = 
        start_queue
        <| (async { do! Async.Sleep 5000 })
        <| create_log
        <| connect_info

    res |> Result.mapFailure (fun e -> logger.Fatal "Could not start main queue: %A" e) |> ignore

    logger.Info "Shutting down..."
        
    0
