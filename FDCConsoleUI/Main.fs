module FDCConcoleUI

open System
open System.Threading

open FDCUtil.Main
open FDCLogger
open FDCNet
open FDCDomain

[<EntryPoint>]
let main argv =
    // let host = "p2p.academ.org"
    let host = "localhost"
    let port = 411

    let logger = new Logger() 
    logger.Info "FDCConsoleUI is starting! args: %A" argv
    
    let agent = Server.startServer { host = host; port = port } (Dcpp.Message.Hello.NickData "MnZrKk")
    Thread.Sleep(5000)

    logger.Info "Shutting down..."
        
    0 // return an integer exit code
