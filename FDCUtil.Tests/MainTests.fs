module FDCUtil.Tests.Main

open System
open System.Threading
open System.Threading.Tasks
open System.Reactive.Concurrency

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper.UnquoteExtensions
open FDCTestHelper.FsCheckExtensions

open FDCUtil.Main

let generate_stream_from_string (s: string) = 
    let stream = new IO.MemoryStream()
    let writer = new IO.StreamWriter(stream)
    writer.Write(s)
    writer.Flush()
    stream.Position <- 0L
    stream

[<Fact>]
let ``Should parse stream`` () = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'

    let sequence = read_message_seq 256 eom_marker stream
    
    let arr = sequence |> Seq.toArray |> Array.map System.Text.Encoding.ASCII.GetString
    
    test <@ arr.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ arr.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ arr.[2] = "$HubName CN.Peers|" @>
    test <@ arr.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream from several pieces`` () = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'

    let sequence = read_message_seq 16 eom_marker stream
    
    let arr = sequence |> Seq.toArray |> Array.map System.Text.Encoding.ASCII.GetString
    
    test <@ arr.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ arr.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ arr.[2] = "$HubName CN.Peers|" @>
    test <@ arr.[3] = "$Hello MnZrKk|" @>  

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
        Thread.Sleep(100)
        lock number_of_times_called (fun () -> number_of_times_called := !number_of_times_called + 1)

    let oncefun = callable_once testfun

    let callfun () =
        oncefun()
        oncefun()
        oncefun()
        oncefun()
        oncefun()
        oncefun()     

    let scheduler = NewThreadScheduler.Default

    use disposable1 = scheduler.Schedule(callfun)
    use disposable2 = scheduler.Schedule(callfun)
    use disposable3 = scheduler.Schedule(callfun)
    use disposable4 = scheduler.Schedule(callfun)

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

module ``Agent Tests`` =
    open System.Threading

    open AgentWithComplexState

    let timeout = 100

    let testF y (x, _) = (x + y, []) |> Success
    let testDivideF y (x, _) = (x / y, []) |> Success
    let testExn _ = failwith "I failed"

    [<Fact>]
    let ``Should create agent`` () =
        let agent = loop (0, []) testF ignore

        test <@ true @>

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
    let ``Should post_and_reply properly`` () =
        let result = loop (10, []) testF (fun agent ->
            agent.post_and_reply 20
        )

        test <@ result = Success (30, []) @>       

    [<Property>]
    let ``Should trigger event only when state changes`` x x' =
        let res = ref None
        do
            loop (x, []) testF 
            <| (fun agent ->
                agent.state_changed 
                |> Event.add (fun ((state, deps), (state', deps')) ->
                    res := testO <@ state <> state' @>
                )
                    
                agent.post x'
            )

        // giving a chance for another thread to update the ref 
        Async.Sleep(1) |> Async.RunSynchronously 
    
        try_raise !res 
        
    [<Fact>]
    let ``Should not trigger events for failure`` () =
        let triggered = ref false
         
        do 
            loop (0, []) 
            <| (fun _ _ -> Failure "test") 
            <| (fun agent -> 
                    agent.state_changed |> Event.add (fun _ -> triggered := true)
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
                    agent.state_changed |> Event.add (fun _ -> triggered := true)
                    agent.post 10
                ) 

        test <@ triggered = ref false @>

    [<Fact>]
    let ``Should get proper error when inner exception`` () =
        
        let res = 
            loop (10, []) testExn
            <| (fun agent -> agent.post_and_reply 10)

        test <@ 
                match res with
                | Failure (ActionException ex) ->
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
                    agent.post_and_reply 2
                )

        test <@ res = Success (5, []) @> 

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
                    agent.post_and_reply x'' |> ignore
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
                    agent.post_and_reply x'' |> ignore
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

                let post_and_reply x = 
                    // not thread-safe
                    let returned = ref 0
                    { new Command<SutType, ModelType>() with
                        override __.RunActual agent = 
                            let res, _ = agent.post_and_reply x |> Result.get
                            returned := res
                            agent
                        override __.RunModel m = m + x
                        override __.Post(agent, m) = !returned = m |@ sprintf "after post_and_reply model: %i <> %i" m !returned
                        override __.ToString() = sprintf "post_and_reply %A" x }

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
                            return post_and_reply elem 
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

    module ``Model-based Tests (experimental version)`` =
        open FsCheck.Experimental

        type SutType = T<int, int*(unit -> unit) list, string>
        type ModelType = int

        [<Property>]
        let ``Should conform to the model`` () =
            let spec create_sut = 
                let post x = 
                    let label = sprintf "post %A" x
                    create_operation label
                    <| (+) x
                    <| (fun agent m -> agent.post x; () |@ label)

                let fetch =
                    create_operation "fetch"
                    <| id
                    <| (fun agent m -> 
                        let res, _ = agent.fetch() |> Result.get
                        res = m |@ sprintf "model: %i <> fetch: %i" m res
                    )

                let post_and_reply x = 
                    create_operation (sprintf "post_and_reply %A" x)
                    <| (+) x
                    <| (fun agent m -> 
                        let res, _ = agent.post_and_reply x |> Result.get
                        res = m |@ sprintf "model: %i <> post_and_reply: %i" m res
                    )
                
                create_machine create_sut (fun _ -> 
                    Gen.oneof [
                            Gen.create1 post
                            Gen.create1 post_and_reply 
                            Gen.constant fetch ])

            let create_sut initial_state = 
                let _, _, sut = 
                    _create 
                    <| (initial_state, [])
                    <| (fun x (state, deps) -> (state+x, deps) |> Success) 
                sut
            
            spec create_sut |> StateMachine.toProperty

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

    