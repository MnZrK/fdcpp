module FDCLogger.Tests.ConsoleLogger

open Xunit
open Swensen.Unquote

open FDCLogger.ConsoleLogger

[<Fact>]
let ``Should construct logger without exceptions`` () = 
    new Logger()
    