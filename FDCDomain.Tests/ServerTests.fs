module FDCDomain.Server.Tests

open Xunit
open Swensen.Unquote

open FDCDomain.Server

[<Fact>]
let ``Should be trivial`` () =
    test <@ 2 + 2 = 4 @>
    
