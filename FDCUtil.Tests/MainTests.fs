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

    let testF y (x, _) = (x + y, []) |> Success
    let testDivideF y (x, _) = (x / y, []) |> Success
    let testExn _ = failwith "I failed"

    [<Fact>]
    let ``Should create agent`` () =
        let agent = AgentWithComplexState.create (0, []) testF

        test <@ true = true @>

    [<Fact>]
    let ``Should sum a list using agent post and fetch it`` () =
        let input = [10; 20; 30; 40; 50]

        let expected = (List.sum input, [])

        let agent = AgentWithComplexState.create (0, []) testF

        input |> List.iter agent.post
        let result = agent.fetch()

        test <@ result = Success expected @>

    [<Fact>]
    let ``Should postAndReply properly`` () =
        let agent = AgentWithComplexState.create (10, []) testF

        let result = agent.postAndReply 20

        test <@ result = Success (Success (30, [])) @>       

    [<Fact>]
    let ``Should trigger events for success`` () =
        let agent = AgentWithComplexState.create (0, []) testF

        let res = ref ((0, []), (0, []))
        agent.event |> Event.add (fun x -> res := x)

        agent.post 10

        test <@ res = ref ((0, []), (10, [])) @>
        
    [<Fact>]
    let ``Should not trigger events for failure`` () =
        let agent = AgentWithComplexState.create (0, []) (fun _ _ -> Failure "test") 

        let triggered = ref false
        agent.event |> Event.add (fun _ -> triggered := true)

        agent.post 10

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should not trigger events for inner exception`` () =
        let agent = AgentWithComplexState.create (0, []) (fun _ _ -> failwith "hello") 

        let triggered = ref false
        agent.event |> Event.add (fun _ -> triggered := true)

        agent.post 10

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should stop the agent`` () =
        let agent = AgentWithComplexState.create (0, []) testF

        agent.stop()

        test <@ agent.post 10 = () @>
        test <@ agent.postAndReply 20 = Failure AgentWithComplexState.Error.IsStopped @>

    [<Fact>]
    let ``Should get proper error when inner exception`` () =
        
        let agent = AgentWithComplexState.create (10, []) testExn

        let res = agent.postAndReply 10 

        test <@ 
                match res with
                | Failure (AgentWithComplexState.OtherError ex) ->
                    match ex.Message with 
                    | "I failed" -> true
                    | _ -> false
                | _ -> false 
            @>

    [<Fact>]
    let ``Should ignore inner exceptions for post`` () =

        let agent = AgentWithComplexState.create (10, []) testDivideF

        agent.post 0

        let res = agent.postAndReply 2 

        test <@ res = Success (Success (5, [])) @> 
