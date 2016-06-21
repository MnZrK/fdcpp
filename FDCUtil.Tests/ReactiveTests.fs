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

let timeout = 100

[<Fact>]
let ``Should await observable`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject
    
    let enumerator = Observable.getEnumerator obs

    subject.OnNext(10)
    test <@ enumerator.MoveNext() = true @>
    let res1 = enumerator.Current
    test <@ res1 = 10 @>

    subject.OnNext(20)
    test <@ enumerator.MoveNext() = true @>
    let res2 = enumerator.Current

    test <@ res2 = 20 @>

    subject.OnNext(30)
    subject.OnNext(40)
    
    test <@ enumerator.MoveNext() = true @>
    let res3 = enumerator.Current
    test <@ res3 = 30 @>
    
    test <@ enumerator.MoveNext() = true @>
    let res4 = enumerator.Current
    test <@ res4 = 40 @>

    // enumerator.MoveNext() |> ignore // this waits infinitely 

[<Fact>]
let ``Should not cancel sync task`` () =
    let action() = 
        Thread.Sleep timeout

    let cts = new CancellationTokenSource()
    let task = Task.Run(action, cts.Token)

    test <@ task.IsCanceled = false @>

    cts.Cancel()

    test <@ task.IsCanceled = false @>

[<Fact>]
let ``Should be executing both subscriptions`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject

    let first_called = ref 0
    let second_called = ref 0
    let main_thread_id = Thread.CurrentThread.ManagedThreadId
    let first_thread_id = ref -1
    let second_thread_id = ref -1

    obs |> Observable.add (fun _ -> 
        first_thread_id := Thread.CurrentThread.ManagedThreadId
        first_called := !first_called + 1)
    obs |> Observable.add (fun _ -> 
        second_thread_id := Thread.CurrentThread.ManagedThreadId
        second_called := !second_called + 1)

    test <@ main_thread_id = Thread.CurrentThread.ManagedThreadId @> 
    
    subject.OnNext(10)
    test <@ main_thread_id = Thread.CurrentThread.ManagedThreadId @>

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ main_thread_id = Thread.CurrentThread.ManagedThreadId @>

    test <@ !first_called = 1 @>
    test <@ !second_called = 1 @>
    test <@ !first_thread_id = !second_thread_id @>

[<Fact>]
let ``Should be calling onCompleted or onError only once`` () =
    let subject = new Subject<int>()
    let obs = Observable.asObservable subject

    let completed_called = ref 0
    let error_called = ref 0

    obs 
    |> Observable.subscribeWithCallbacks 
        ignore
        (fun _ -> error_called := !error_called + 1) 
        (fun _ -> completed_called := !completed_called + 1) 
    |> ignore

    subject.OnCompleted()
    subject.OnError(exn "failed")
    subject.OnCompleted()

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !completed_called + !error_called = 1 @>

[<Fact>]
let ``Should not be executing onNext after onError`` () =
    let subject = new Subject<int>()

    let obs = Observable.asObservable subject
    
    let got_error = ref false
    let failed = ref false
    let oncompleted_called = ref false

    let dispose = 
        obs 
        |> Observable.subscribeWithCallbacks
            (fun x -> if !got_error then failed := true)
            (fun x -> got_error := true)
            (fun _ -> oncompleted_called := true)
    
    subject.OnNext(1)
    subject.OnNext(2)
    subject.OnNext(3)
    subject.OnNext(4)
    subject.OnError(exn "got an error")
    subject.OnNext(5)
    subject.OnNext(6)

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !failed = false @>
    test <@ !got_error = true @>
    test <@ !oncompleted_called = false @>

[<Fact>]
let ``Should not be executing onNext after onCompleted`` () =
    let subject = new Subject<int>()

    let obs = Observable.asObservable subject
    
    let completed = ref false
    let failed = ref false
    let onerror_called = ref false

    let dispose = 
        obs 
        |> Observable.subscribeWithCallbacks
            (fun x -> if !completed then failed := true)
            (fun _ -> onerror_called := true)
            (fun x -> completed := true)
    
    subject.OnNext(1)
    subject.OnNext(2)
    subject.OnNext(3)
    subject.OnNext(4)
    subject.OnCompleted()
    subject.OnNext(5)
    subject.OnNext(6)

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !failed = false @>
    test <@ !completed = true @>
    test <@ !onerror_called = false @>

[<Fact>]
let ``Should not be executing onNext after dispose`` () =
    let subject = new Subject<int>()

    let obs = Observable.asObservable subject
    
    let disposed = ref false
    let failed = ref false
    let onerror_called = ref false
    let oncompleted_called = ref false

    let dispose = 
        obs 
        |> Observable.subscribeWithCallbacks
            (fun x -> if !disposed then failed := true)
            (fun _ -> onerror_called := true)
            (fun _ -> oncompleted_called := true)
    
    subject.OnNext(1)
    subject.OnNext(2)
    subject.OnNext(3)
    subject.OnNext(4)
    dispose.Dispose()
    disposed := true
    subject.OnNext(5)
    subject.OnNext(6)

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !failed = false @>
    test <@ !onerror_called = false @>
    test <@ !oncompleted_called = false @>


[<Fact>]
let ``Should not be executing onNext after onError with add`` () =
    let subject = new Subject<int>()

    let obs = Observable.asObservable subject
    
    let got_error = ref false
    let failed = ref false
    let oncompleted_called = ref false

    let dispose = 
        obs 
        |> Observable.asUpdates
        |> Observable.add (
            function
            | Next _ -> if !got_error then failed := true
            | Error _ -> got_error := true
            | Completed -> oncompleted_called := true
        )
    
    subject.OnNext(1)
    subject.OnNext(2)
    subject.OnNext(3)
    subject.OnNext(4)
    subject.OnError(exn "got an error")
    subject.OnNext(5)
    subject.OnNext(6)

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !failed = false @>
    test <@ !got_error = true @>
    test <@ !oncompleted_called = false @>

[<Fact>]
let ``Should not be executing onNext after onCompleted with add`` () =
    let subject = new Subject<int>()

    let obs = Observable.asObservable subject
    
    let completed = ref false
    let failed = ref false
    let onerror_called = ref false

    let dispose = 
        obs 
        |> Observable.asUpdates
        |> Observable.add (
            function
            | Next _ -> if !completed then failed := true
            | Error _ -> onerror_called := true
            | Completed -> completed := true
        )
        
    subject.OnNext(1)
    subject.OnNext(2)
    subject.OnNext(3)
    subject.OnNext(4)
    subject.OnCompleted()
    subject.OnNext(5)
    subject.OnNext(6)

    Async.Sleep(timeout) |> Async.RunSynchronously

    test <@ !failed = false @>
    test <@ !completed = true @>
    test <@ !onerror_called = false @>

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
    
    let obs, dispose_obs = Array.fetchConcatSplit None eom_marker read_async
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
    
    let obs, dispose_obs = Array.fetchConcatSplit None eom_marker read_async
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
    
    let obs, dispose_obs = Array.fetchConcatSplit None eom_marker read_async
    let msgs = Observable.toArray obs |> Async.AwaitObservable |> Async.RunSynchronously |> Array.map System.Text.Encoding.ASCII.GetString

    test <@ msgs.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ msgs.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ msgs.[2] = "$HubName CN.Peers|" @>
    test <@ msgs.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream using AsyncRead with large buffer`` () = ``Should parse stream using AsyncRead`` 512

[<Fact>]
let ``Should parse stream using AsyncRead with small buffer`` () = ``Should parse stream using AsyncRead`` 16
