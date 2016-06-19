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
let ``Should not throw when reading more then content`` () =
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input

    let read_async () = Async.Catch(stream.AsyncRead(512)) |> Async.map Result.ofChoice2

    let read = read_async() |> Async.RunSynchronously

    test <@ read |> Result.isSuccess |> not @>

let ``Should parse stream`` buffer_size = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'

    let read_async () = Async.Catch(stream.AsyncRead(buffer_size)) |> Async.map Result.ofChoice2
    
    let obs = Array.fetchConcatSplit eom_marker read_async ((new CancellationTokenSource()).Token)
    Observable.subscribeWithCallbacks (fun x -> printfn "onNext: %A" x) (fun x -> printfn "onError: %A" x) (fun x -> printfn "onCompletion: %A" x) |> ignore

    Thread.Sleep 5000

    test <@ false @>
    // let msgs = Observable.toArray obs |> Async.AwaitObservable |> Async.RunSynchronously |> Array.map System.Text.Encoding.ASCII.GetString

    // test <@ msgs.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    // test <@ msgs.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    // test <@ msgs.[2] = "$HubName CN.Peers|" @>
    // test <@ msgs.[3] = "$Hello MnZrKk|" @>  

// TODO fix
// [<Fact>]
// let ``Should parse stream when buffer is more than message`` () = ``Should parse stream`` 512

// [<Fact>]
// let ``Should parse stream when buffer is less than message`` () = ``Should parse stream`` 16 
