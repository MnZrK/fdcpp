module FDCLogger.Tests.ConsoleLogger

open Xunit
open Swensen.Unquote

open FDCLogger.ConsoleLogger

[<Fact>]
let ``Should construct logger without exceptions`` () = 
    new Logger()
    
[<Fact>]
let ``Should log without exceptions`` () = 
    let logger = new Logger()
    logger.Trace "hello world %s" "another hello world"