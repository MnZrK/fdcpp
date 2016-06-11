module FDCLogger.Tests.NLogLogger

open Xunit
open FDCLogger.NLogLogger

[<Fact>]
let ``Should construct logger without exceptions`` () = 
    new Logger()

