module FDCNet.Tests.Util

open Xunit
open Swensen.Unquote

open FDCUtil.Main
open FDCNet.Util

[<Fact>]
let ``Should get bytes from ASCII string`` () =
    test <@ getBytes "123" = [| 49uy; 50uy; 51uy |] @>
        
[<Fact>]
let ``Should get string from ASCII bytes`` () =
    test <@ getString [| 49uy; 50uy; 51uy |] = "123" @>
        
[<Fact>]
let ``Should convert dangerous characters to DCN`` () =
    test <@ 
            "12$3" 
            |> getBytes 
            |> Array.collect byteToDCN 
            |> getString = "12/%DCN036%/3" 
        @>
    
[<Fact>]
let ``Should not touch input wihout dangerous characters`` () =
    let input = "123abbss"

    test <@ 
            input 
            |> getBytes 
            |> Array.collect byteToDCN 
            |> getString = input 
        @>

[<Fact>]
let ``Should convert the same for bytes and strings`` () =
    let inputStr = "123abbss"
    let inputBytes = getBytes "123abbss"

    test <@ 
            inputBytes
            |> Array.collect byteToDCN
            |> getString = stringToDCN inputStr 
        @>

[<Fact>]
let ``Should convert DCN string back to normal`` () =
    let input = "12/%DCN036%/3" 

    let expected = "12$3"

    test <@ DCNtoString input = expected @>

[<Fact>]
let ``Should convert DCN back and worth without change`` () =
    let input = "12$3|\01235ffffaa"

    test <@ "12$3|\01235ffffaa" |> stringToDCN |> DCNtoString = "12$3|\01235ffffaa" @>
    