module FDCUtil.Tests.Main

open System
open System.Threading
open System.Threading.Tasks

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper

open FDCUtil.Main

[<Fact>]
let ``Should call once and only once function produced by 'callable_once'`` () =
    let number_of_times_called = ref 0

    let testfun () = 
        number_of_times_called := !number_of_times_called + 1

    testfun()
    test <@ !number_of_times_called = 1 @>
    testfun()
    test <@ !number_of_times_called = 2 @>

    let oncefun = callable_once testfun

    oncefun()
    test <@ !number_of_times_called = 3 @>
    oncefun()
    test <@ !number_of_times_called = 3 @>

    testfun()
    test <@ !number_of_times_called = 4 @>

    oncefun()
    test <@ !number_of_times_called = 4 @>

[<Fact>]
let ``Should call once and only once function produced by 'callable_once' even in parallel`` () =
    let number_of_times_called = ref 0

    let testfun () = 
        lock number_of_times_called (fun () -> number_of_times_called := !number_of_times_called + 1)

    let oncefun = callable_once testfun

    let callfun () = async {
        do! Async.Sleep(100)
        oncefun()
        oncefun()
        oncefun()
        oncefun()
        oncefun()
        oncefun()     
    }

    let asyncs = 
        [ callfun()
        ; callfun()
        ; callfun()
        ; callfun()
        ; callfun()
        ; callfun()
        ; callfun()
        ]

    Async.Parallel asyncs |> Async.RunSynchronously |> ignore

    Thread.Sleep(200)

    test <@ !number_of_times_called = 1 @>

module ``String Tests`` =
    [<Fact>]
    let ``Should split example strings`` () =
        let res = List.ofSeq << (String.split "$$")

        test <@ res "first$$second$$third" = ["first"; "second"; "third"] @>
        test <@ res "first$$second$$third$$" = ["first"; "second"; "third"] @>
        test <@ res "$$first$$second$$third" = [""; "first"; "second"; "third"] @>
        test <@ res "$$first$$second$$third$$" = [""; "first"; "second"; "third"] @>

module ``Result Tests`` =
    open Result

    let testFun x = 
        if x > 10 then
            Success x
        else
            Failure (sprintf "%i is less than 10" x)

    [<Fact>]
    let ``Should work with success_workflow and success values`` () =
        let xinput = 15
        let yinput = 20

        let expected = xinput + yinput

        let result = success_workflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Success expected @>
        
    [<Fact>]
    let ``Should work with success_workflow and first failure value`` () =
        let xinput = 5
        let yinput = 20

        let result = success_workflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Failure "5 is less than 10" @>
        
    [<Fact>]
    let ``Should work with success_workflow and second failure value`` () =
        let xinput = 15
        let yinput = 6

        let result = success_workflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Failure "6 is less than 10" @>
