module FDCUtil.Tests.Reactive

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Reactive
open FSharpx.Control
open FSharpx.Control.Observable

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper

open FDCUtil.Main
open FDCUtil.Reactive

[<Fact>]
let ``Should not throw (but should fail) when reading more then content`` () =
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input

    let read_async () = Async.Catch(stream.AsyncRead(input.Length + 1)) |> Async.map Result.ofChoice2

    let read = read_async() |> Async.RunSynchronously

    test <@ read |> Result.isSuccess |> not @>

[<Fact>]
let ``Should not invoke deferred observable if not subscribed`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject
    let invoked = ref false

    let deferred_obs = Observable.defer (fun () ->
        invoked := true
        obs
    )

    Async.Sleep 1 |> Async.RunSynchronously 

    test <@ !invoked = false @>

[<Fact>]
let ``Should invoke deferred observable when subscribed`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject
    let invoked = ref false

    let deferred_obs = Observable.defer (fun () ->
        invoked := true
        obs
    )

    deferred_obs |> Observable.subscribe ignore |> ignore

    Async.Sleep 1 |> Async.RunSynchronously 

    test <@ !invoked = true @>

[<Fact>]
let ``Should invoke deferred observable when subscribed with callbacks`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject
    let invoked = ref false

    let deferred_obs = Observable.defer (fun () ->
        invoked := true
        obs
    )

    deferred_obs |> Observable.subscribeWithCallbacks ignore ignore ignore |> ignore

    Async.Sleep 1 |> Async.RunSynchronously 

    test <@ !invoked = true @>

let ``Should parse stream using async wrapper around sync read`` buffer_size = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'

    let read_async () = async {
        let buffer = Array.zeroCreate buffer_size
        return
            try
                stream.Read(buffer, 0, buffer_size) |> Success
            with ex ->
                Failure ex 
            |> Result.map (fun bytes_read -> 
                if bytes_read <= 0 then 
                    [||]
                else 
                    buffer.[0..bytes_read-1]
            )
    }
    
    let obs = Array.fetchConcatSplit eom_marker read_async ((new CancellationTokenSource()).Token)
    let msgs = Observable.toArray obs |> Async.AwaitObservable |> Async.RunSynchronously |> Array.map System.Text.Encoding.ASCII.GetString

    test <@ msgs.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ msgs.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ msgs.[2] = "$HubName CN.Peers|" @>
    test <@ msgs.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream using async wrapper around sync read with large buffer`` () = ``Should parse stream using async wrapper around sync read`` 512

[<Fact>]
let ``Should parse stream using async wrapper around sync read with small buffer`` () = ``Should parse stream using async wrapper around sync read`` 16

let ``Should parse stream using ReadAsync`` buffer_size = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'
    let cts = new CancellationTokenSource()

    let read_async () = async {
        let buffer = Array.zeroCreate buffer_size
        let! read = Async.Catch(stream.ReadAsync(buffer, 0, buffer_size, cts.Token) |> Async.AwaitTask)
        let res =
            Result.ofChoice2 read
            |> Result.map (fun bytes_read ->
                if bytes_read <= 0 then [||]
                else buffer.[0..bytes_read-1]
            )
        return res
    }
    
    let obs = Array.fetchConcatSplit eom_marker read_async ((new CancellationTokenSource()).Token)
    let msgs = Observable.toArray obs |> Async.AwaitObservable |> Async.RunSynchronously |> Array.map System.Text.Encoding.ASCII.GetString

    test <@ msgs.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ msgs.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ msgs.[2] = "$HubName CN.Peers|" @>
    test <@ msgs.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream using ReadAsync with large buffer`` () = ``Should parse stream using ReadAsync`` 512

[<Fact>]
let ``Should parse stream using ReadAsync with small buffer`` () = ``Should parse stream using ReadAsync`` 16

let ``Should parse stream using AsyncRead`` buffer_size = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'
    let cts = new CancellationTokenSource()

    let read_async () = async {
        let buffer = Array.zeroCreate buffer_size
        let! read = Async.Catch(stream.AsyncRead(buffer, 0, buffer_size))
        let res =
            Result.ofChoice2 read
            |> Result.map (fun bytes_read ->
                if bytes_read <= 0 then [||]
                else buffer.[0..bytes_read-1]
            )
        return res
    }
    
    let obs = Array.fetchConcatSplit eom_marker read_async ((new CancellationTokenSource()).Token)
    let msgs = Observable.toArray obs |> Async.AwaitObservable |> Async.RunSynchronously |> Array.map System.Text.Encoding.ASCII.GetString

    test <@ msgs.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ msgs.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ msgs.[2] = "$HubName CN.Peers|" @>
    test <@ msgs.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream using AsyncRead with large buffer`` () = ``Should parse stream using AsyncRead`` 512

[<Fact>]
let ``Should parse stream using AsyncRead with small buffer`` () = ``Should parse stream using AsyncRead`` 16
