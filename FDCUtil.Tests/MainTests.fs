module FDCUtil.Tests.Main

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCUtil.Tests.UnquoteExtensions

open FDCUtil.Main

open System.Threading

module ``Result Tests`` =
    open Result

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

        let result = successWorkflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Success expected @>
        
    [<Fact>]
    let ``Should work with successWorkflow and first failure value`` () =
        let xinput = 5
        let yinput = 20

        let result = successWorkflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Failure "5 is less than 10" @>
        
    [<Fact>]
    let ``Should work with successWorkflow and second failure value`` () =
        let xinput = 15
        let yinput = 6

        let result = successWorkflow {
            let! x = testFun xinput
            let! y = testFun yinput
            return x + y
        }

        test <@ result = Failure "6 is less than 10" @>

module ``Agent Tests`` =
    open System.Threading

    open AgentWithComplexState

    let timeout = 100

    let testF y (x, _) = (x + y, []) |> Success
    let testDivideF y (x, _) = (x / y, []) |> Success
    let testExn _ = failwith "I failed"

    [<Fact>]
    let ``Should create agent`` () =
        let agent = loop (0, []) testF (fun agent -> ())

        test <@ true = true @>

    [<Fact>]
    let ``Should sum a list using agent post and fetch it`` () =
        let input = [10; 20; 30; 40; 50]

        let expected = (List.sum input, [])

        let result = loop (0, []) testF (fun agent -> 
            input |> List.iter agent.post
            agent.fetch()    
        )

        test <@ result = Success expected @>

    [<Fact>]
    let ``Should postAndReply properly`` () =
        let result = loop (10, []) testF (fun agent ->
            agent.postAndReply 20
        )

        test <@ result = Success (Success (30, [])) @>       

    [<Property>]
    let ``Should trigger event only when state changes`` x x' =
        let state_should_change = x' <> 0

        let res = ref None
        do
            loop (x, []) testF 
            <| (fun agent ->
                agent.event 
                |> Event.add (fun ((state, deps), (state', deps')) ->
                    res := testO <@ state <> state' @>
                )
                    
                agent.post x'
            )

        // giving a chance for another thread to update the ref 
        Async.Sleep(1) |> Async.RunSynchronously 
    
        tryRaise !res 
        
    [<Fact>]
    let ``Should not trigger events for failure`` () =
        let triggered = ref false
         
        do 
            loop (0, []) 
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
            loop (0, []) 
            <| (fun _ _ -> failwith "hello")
            <| (fun agent -> 
                    agent.event |> Event.add (fun _ -> triggered := true)
                    agent.post 10
                ) 

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should get proper error when inner exception`` () =
        
        let res = 
            loop (10, []) testExn
            <| (fun agent -> agent.postAndReply 10)

        test <@ 
                match res with
                | Failure (OtherError ex) ->
                    match ex.Message with 
                    | "I failed" -> true
                    | _ -> false
                | _ -> false 
            @>

    [<Fact>]
    let ``Should ignore inner exceptions for post`` () =

        let res = 
            loop (10, []) testDivideF
            <| (fun agent -> 
                    agent.post 0
                    agent.postAndReply 2
                )

        test <@ res = Success (Success (5, [])) @> 

    type MyAction = float
    type MyState = int
    type MyDeps = (unit -> unit) list
    type MyError = string
    type MyResult = Result<MyState * MyDeps, MyError>

    [<Property>]
    let ``Should not raise exceptions with random function`` (x: MyState) (deps: MyDeps) (x': MyAction) (x'': MyAction) (f: MyAction -> MyState*MyDeps -> MyResult) =
        let failF a (s, d) = failwith "I failed"

        do
            loop (x, deps) f
            <| (fun agent ->
                    agent.post x'
                    agent.fetch() |> ignore
                    agent.postAndReply x'' |> ignore
                    agent.fetch() |> ignore
                )

    [<Property>]
    let ``Should not raise exceptions with fail function`` (x: MyState) (deps: MyDeps) (x': MyAction) (x'': MyAction) =
        do
            loop (x, deps) 
            <| (fun _ _ -> failwith "I failed")
            <| (fun agent ->
                    agent.post x'
                    agent.fetch() |> ignore
                    agent.postAndReply x'' |> ignore
                    agent.fetch() |> ignore
                )
    module ``Model-based Tests`` =
        type SutType = T<int, int*(unit -> unit) list, string>
        type ModelType = int

        [<Fact>]
        let ``Should conform to the model`` () =
            let spec sutConstructor initialModel = 
                let post x = { 
                    new Command<SutType, ModelType>() with
                        override __.RunActual agent = agent.post x; agent
                        override __.RunModel m = m + x
                        override __.Post(agent, m) = 
                            let state, _ = agent.fetch() |> Result.get
                            state = m |@ sprintf "after post model: %i <> %i" m state
                        override __.ToString() = sprintf "post %A" x }
                        
                let fetch () = 
                    // not thread-safe
                    let returned = ref 0
                    { new Command<SutType, ModelType>() with
                        override __.RunActual agent = 
                            let res, _ = (agent.fetch() |> Result.get)
                            returned := res 
                            agent
                        override __.RunModel m = m
                        override __.Post(agent, m) = !returned = m |@ sprintf "after fetch model: %i <> %i" m !returned
                        override __.ToString() = "fetch" }

                let postAndReply x = 
                    // not thread-safe
                    let returned = ref 0
                    { new Command<SutType, ModelType>() with
                        override __.RunActual agent = 
                            let res, _ = (agent.postAndReply x |> Result.get |> Result.get)
                            returned := res
                            agent
                        override __.RunModel m = m + x
                        override __.Post(agent, m) = !returned = m |@ sprintf "after postAndReply model: %i <> %i" m !returned
                        override __.ToString() = sprintf "postAndReply %A" x }

                { new ICommandGenerator<SutType, ModelType> with
                    member __.InitialActual = sutConstructor()
                    member __.InitialModel = initialModel
                    member __.Next model = 
                        let postGen = gen { 
                            let! elem = Arb.generate<int>
                            return post elem 
                        }
                        let postAndReplyGen = gen {
                            let! elem = Arb.generate<int>
                            return postAndReply elem 
                        }
                        let fetchGen = gen {
                            return fetch()
                        }
                        Gen.oneof [ postGen; postAndReplyGen; fetchGen ]
                    }

            let initialState = 0

            let sutConstructor () = 
                let _, _, sut = 
                    _create 
                    <| (initialState, [])
                    <| (fun x (state, deps) -> (state+x, deps) |> Success) 
                sut
            
            let prop = spec sutConstructor initialState |> Command.toProperty

            Check.One ({Config.QuickThrowOnFailure with QuietOnSuccess = true }, prop)

module ``Regex Tests`` =
    open Regex

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
