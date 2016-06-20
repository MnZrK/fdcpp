module FDCUtil.Tests.AgentWithComplexStateTests

open System
open System.Threading
open System.Threading.Tasks

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper

open FDCUtil
open FDCUtil.AgentWithComplexState

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
            |> Observable.add (fun ((state, deps), (state', deps')) ->
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
                agent.state_changed |> Observable.add (fun _ -> triggered := true)
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
                agent.state_changed |> Observable.add (fun _ -> triggered := true)
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
            | Failure (ActionError (ActionException ex)) ->
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
