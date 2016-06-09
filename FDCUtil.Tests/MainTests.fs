module FDCUtil.Main.Tests

open Xunit
open Swensen.Unquote

open FDCUtil.Main

module ``Result Tests`` =

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

module ``Agent Tests`` =
    open System.Threading

    let testF = (+)
    let testFDelay x y =
        Thread.Sleep 1000
        x + y  

    [<Fact>]
    let ``Should create agent`` () =
        let cts = new CancellationTokenSource()

        let agent = Agent.create cts.Token 0 testF

        test <@ true = true @>

    [<Fact>]
    let ``Should sum a list using agent post and fetch it`` () =
        let cts = new CancellationTokenSource()

        let input = [10; 20; 30; 40; 50]

        let expected = List.sum input

        let agent = Agent.create cts.Token 0 testF

        input |> List.iter agent.post
        let result = agent.fetch()

        test <@ result = expected @>

    [<Fact>]
    let ``Should sum a list using agent post and fetchAsync it`` () =
        let cts = new CancellationTokenSource()

        let input = [10; 20; 30; 40; 50]

        let expected = List.sum input

        let agent = Agent.create cts.Token 0 testF

        input |> List.iter agent.post
        let result = agent.fetchAsync() |> Async.RunSynchronously

        test <@ result = expected @>

    [<Fact>]
    let ``Should sum a list using agent postAndReply`` () =
        let cts = new CancellationTokenSource()

        let input = [10; 20; 30; 40; 50]

        let expected = [10; 30; 60; 100; 150]

        let agent = Agent.create cts.Token 0 testF

        let result = input |> List.map (fun x -> let _, y = agent.postAndReply x in y)

        test <@ result = expected @>

    [<Fact>]
    let ``Should sum a list using agent postAndReplyAsync`` () =
        let cts = new CancellationTokenSource()

        let input = [10; 20; 30; 40; 50]

        let expected = [10; 30; 60; 100; 150]

        let agent = Agent.create cts.Token 0 testF

        let asyncs = input |> List.map (fun x ->
            async {
                let! _, y = agent.postAndReplyAsync x
                return y
            } 
            ) 
        let result = asyncs |> List.map Async.RunSynchronously

        test <@ result = expected @>
