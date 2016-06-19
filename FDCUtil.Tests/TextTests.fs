module FDCUtil.Tests.Text

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper

open FDCUtil.Text

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
    
[<Property>]
let ``Should not raise any exceptions`` =
    function
    | Regex "^hello (.*) my (.*)$" _ -> true
    | _ -> true

[<Fact>]
let ``Should not match regex with incorrect string`` () =
    let input = "hell world my friends"
    let r = "^hello (.*) my (.*)$"
    
    let result = 
        match input with
        | Regex r l -> Some l
        | _ -> None

    test <@ result = None @>

