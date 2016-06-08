module FDCUtil.Main.Tests

open Xunit
open Swensen.Unquote

open FDCUtil.Main

let testFun x = 
    if x > 10 then
        Success x
    else
        Failure (sprintf "%i is less than 10" x)

[<Fact>]
let ``Should work with successWorkflow and success values`` () =
    let xinput = 15
    let yinput = 20

    let expected = xinput + yinput

    let result = Result.successWorkflow {
        let! x = testFun xinput
        let! y = testFun yinput
        return x + y
    }

    test <@ result = Success expected @>
    
[<Fact>]
let ``Should work with successWorkflow and first failure value`` () =
    let xinput = 5
    let yinput = 20

    let result = Result.successWorkflow {
        let! x = testFun xinput
        let! y = testFun yinput
        return x + y
    }

    test <@ result = Failure "5 is less than 10" @>
    
[<Fact>]
let ``Should work with successWorkflow and second failure value`` () =
    let xinput = 15
    let yinput = 6

    let result = Result.successWorkflow {
        let! x = testFun xinput
        let! y = testFun yinput
        return x + y
    }

    test <@ result = Failure "6 is less than 10" @>
