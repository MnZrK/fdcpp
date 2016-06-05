module FDCConcoleUI

open System
open System.Threading
open FDCLogger
open FDCNet

[<EntryPoint>]
let main argv =
    let logger = new Logger()
    logger.Trace "Hello world from fundcpp console! args: %A" argv
    logger.Trace "result from library: %d" (FDCNet.Main.my_main 5)
    logger.Info "Starting TCP server..."
    
    use disposable = Tcp.Server.Start(port = 8090)
    Thread.Sleep(60 * 1000)
    logger.Info "Shutting down..."
        
    0 // return an integer exit code
