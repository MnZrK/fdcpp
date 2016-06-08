module FDCUtil.Regex.Tests

open Xunit
open Swensen.Unquote

open FDCUtil.Regex

[<Fact>]
let ``Should match regex with correct string`` () =
    let input = "hello world my friends"
    let r = "^hello (.*) my (.*)$"
    
    let expected = ["world"; "friends"]

    let result = 
        match input with
        | Regex r l -> Some l
        | _ -> None

    test <@ result = Some expected @>
    
[<Fact>]
let ``Should not match regex with incorrect string`` () =
    let input = "hell world my friends"
    let r = "^hello (.*) my (.*)$"
    
    let result = 
        match input with
        | Regex r l -> Some l
        | _ -> None

    test <@ result = None @>
    
