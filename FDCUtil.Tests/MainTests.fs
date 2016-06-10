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
        let agent = AgentWithComplexState.loop (0, []) testF (fun agent -> ())

        test <@ true = true @>

    [<Fact>]
    let ``Should sum a list using agent post and fetch it`` () =
        let input = [10; 20; 30; 40; 50]

        let expected = (List.sum input, [])

        let result = AgentWithComplexState.loop (0, []) testF (fun agent -> 
            input |> List.iter agent.post
            agent.fetch()    
        )

        test <@ result = Success expected @>

    [<Fact>]
    let ``Should postAndReply properly`` () =
        let result = AgentWithComplexState.loop (10, []) testF (fun agent ->
            agent.postAndReply 20
        )

        test <@ result = Success (Success (30, [])) @>       

    [<Fact>]
    let ``Should trigger events for success`` () =
        let res = ref ((0, []), (0, []))

        do AgentWithComplexState.loop (0, []) testF (fun agent ->
            agent.event |> Event.add (fun x -> res := x)
            agent.post 10
        )

        test <@ res = ref ((0, []), (10, [])) @>
        
    [<Fact>]
    let ``Should not trigger events for failure`` () =
        let triggered = ref false
         
        do 
            AgentWithComplexState.loop (0, []) 
            <| (fun _ _ -> Failure "test") 
            <| (fun agent -> 
                    agent.event |> Event.add (fun _ -> triggered := true)
                    agent.post 10
                )

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should not trigger events for inner exception`` () =
        let triggered = ref false
        
        do
            AgentWithComplexState.loop (0, []) 
            <| (fun _ _ -> failwith "hello")
            <| (fun agent -> 
                    agent.event |> Event.add (fun _ -> triggered := true)
                    agent.post 10
                ) 

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should get proper error when inner exception`` () =
        
        let res = 
            AgentWithComplexState.loop (10, []) testExn
            <| (fun agent -> agent.postAndReply 10)

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

        let res = 
            AgentWithComplexState.loop (10, []) testDivideF
            <| (fun agent -> 
                    agent.post 0
                    agent.postAndReply 2
                )

        test <@ res = Success (Success (5, [])) @> 
