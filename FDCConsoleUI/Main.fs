module FDCConcoleUI.Main

open System
open System.Threading

open FSharp.Control.Reactive

open FDCConsoleUI.Infrastructure
open FDCUtil.Main

// [<EntryPoint>]
// let main argv =
//     printfn "Starting... %A" Thread.CurrentThread.ManagedThreadId

//     let observable = (Observable.publish << Observable.ofAsync) (async {
//         printfn "Doing inside observable"
//         do! Async.Sleep 2000
//         return 10
//     })
//     use disposable_initial = Observable.connect observable

//     // let observable = Observable.ofSeqOn System.Reactive.Concurrency.NewThreadScheduler.Default (seq {
//     //     printfn "Doing inside observable %A" Thread.CurrentThread.ManagedThreadId
//     //     Thread.Sleep 2000
//     //     yield 10
//     //     Thread.Sleep 2000
//     //     yield 20
//     // })
//     // let observable = Builders.observe {
//     //     Thread.Sleep 2000
//     //     yield 10
//     //     Thread.Sleep 2000
//     //     yield 20
//     // }

//     use disposable = 
//         observable
//         |> (Observable.subscribeWithCallbacks 
//             <| (fun x -> printfn "onNext1 %A %A" Thread.CurrentThread.ManagedThreadId x)
//             <| (fun x -> printfn "onError1 %A %A" Thread.CurrentThread.ManagedThreadId x)
//             <| (fun x -> printfn "onCompleted1 %A %A" Thread.CurrentThread.ManagedThreadId x))

//     use disposable2 = 
//         observable
//         |> (Observable.subscribeWithCallbacks 
//             <| (fun x -> printfn "onNext2 %A %A" Thread.CurrentThread.ManagedThreadId x)
//             <| (fun x -> printfn "onError2 %A %A" Thread.CurrentThread.ManagedThreadId x)
//             <| (fun x -> printfn "onCompleted2 %A %A" Thread.CurrentThread.ManagedThreadId x))

//     printfn "Finished setup %A" Thread.CurrentThread.ManagedThreadId

//     Thread.Sleep 5000

//     printfn "Exiting..."
//     0

[<EntryPoint>]
let main argv =
    printfn "Starintg %A" Thread.CurrentThread.ManagedThreadId
    let eom_marker = Convert.ToByte '|'
    let client = TcpStreamClient.start eom_marker "localhost" 411 |> Async.RunSynchronously
    // Thread.Sleep 2000

    printfn "Observable.add1 %A" Thread.CurrentThread.ManagedThreadId
    use disposable = 
        client.Received
        // |>! Observable.add (fun x -> printfn "Observable.add %A %A" Thread.CurrentThread.ManagedThreadId x)
        |> (Observable.subscribeWithCallbacks 
            <| (fun x -> printfn "onNext 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
            <| (fun x -> printfn "onError 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
            <| (fun x -> printfn "onCompleted 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
        ) 
    printfn "finished setup 1 %A" Thread.CurrentThread.ManagedThreadId

    // Thread.Sleep 2000
    
    printfn "Observable.add2 %A" Thread.CurrentThread.ManagedThreadId
    use anotherdisposable = 
        client.Received
            |> (Observable.subscribeWithCallbacks 
                <| (fun x -> printfn "onNext 2 %A %A" Thread.CurrentThread.ManagedThreadId x)
                <| (fun x -> printfn "onError 2 %A %A" Thread.CurrentThread.ManagedThreadId x)
                <| (fun x -> printfn "onCompleted 2 %A %A" Thread.CurrentThread.ManagedThreadId x)

            )
    printfn "finished setup 2 %A" Thread.CurrentThread.ManagedThreadId

    (client :> IDisposable).Dispose()

    Thread.Sleep 60000

    0

// type Client = {
//     /// function to close connection
//     dispose: unit -> unit
//     /// event for received messages
//     receivedEvent: IEvent<byte[]>
//     /// Write function to send replies. If connection is already closed, calling
//     /// this function won't trigger any errors, but nothing will happen as well.
//     write: byte[] -> unit
// }

// /// It assumes that the message is received when `checkMsgReceived` argument returns true.
// let startClient (checkMsgReceived: byte list * byte -> bool) (hostname: string) (port: int) =
//     let logger = new Logger()
//     let receivedCommand = new Event<byte[]>()

//     let client = new System.Net.Sockets.TcpClient()

//     // do! client.ConnectAsync(hostname, port) |> Async.AwaitIAsyncResult |> Async.Ignore
//     // TODO fix async 
//     logger.Info "Connecting to (%A %A) ..." hostname port
//     client.Connect(hostname, port)
//     logger.Info "Connected to %A %A" hostname port //TODO what if host is not available
//     let stream = client.GetStream()
    
//     // TODO use list or something instead of byte[]
//     let rec asyncReadingLoop (message: byte list) (stream : NetworkStream) = async {
//         logger.Trace "Getting byte"
//         // do! Async.Sleep 100
//         let! bytes = 
//             try 
//                 stream.AsyncRead(1)
//             with ex -> 
//                 logger.Error "WTFWTFWTF"
//                 Thread.Sleep(1000)
//                 raise ex
//         logger.Trace "Got byte"
//         if checkMsgReceived(message, bytes.[0]) then
//             logger.Trace "Triggering `ReceivedCommand` event"
//             receivedCommand.Trigger(List.toArray message)
//             return! asyncReadingLoop [] stream
//         else
//             return! asyncReadingLoop (List.append message (List.ofArray bytes)) stream 
//     }
    
//     let cts = new CancellationTokenSource()
//     Async.Start(asyncReadingLoop [] stream, cancellationToken = cts.Token)
//     let dispose() =  
//         logger.Info "Disposing..." 
//         cts.Cancel() 
//         client.Close() // TODO check if `Close` is happenning after all tasks related to cts are cancelled 
        
//     // TODO make proper cancellation for agent 
//     let agent = MailboxProcessor.Start(fun inbox -> 
//         let rec asyncWritingLoop() = async {
//             let! msg = inbox.Receive()
            
//             logger.Trace "Sending response..."
//             try 
//                 do! stream.WriteAsync(msg, 0, Array.length msg, cts.Token) |> Async.AwaitIAsyncResult |> Async.Ignore
//             with ex ->
//                 logger.Error "OMGOMGOMG"
//             logger.Trace "Sent response"

//             return! asyncWritingLoop()
//         }
//         asyncWritingLoop()
//     , cts.Token)
    
//     {dispose = dispose; receivedEvent = receivedCommand.Publish; write = agent.Post}


// let create_log () = (new Logger() :> ILogger)
// let create_transport connect_info = 
//     let start_dcpp_client = startClient (fun (msg, byte) -> byte = Convert.ToByte '|')
//     let parseDcppMessage input =
//         Result.success_workflow_with_string_failures {
//             match input with
//             | Regex "^\$Hello (.*)$" [ nick ] ->
//                 let! nick_data = NickData.create nick
//                 return DcppReceiveMessage.Hello { nick = nick_data }
//             | Regex "^\$BadPass$" [] -> 
//                 return DcppReceiveMessage.BadPass
//             | Regex "^\$GetPass$" [] -> 
//                 return DcppReceiveMessage.GetPass
//             | Regex "^\$ValidateDenide$" [] -> 
//                 return DcppReceiveMessage.ValidateDenied
//             | Regex "^\$Lock (.*) Pk=(.*)$" [ lock; pk ] ->
//                 let! lock_data = LockData.create << DCNtoString <| lock
//                 // TODO check for what fields DCN encoding/decoding should be happening 
//                 let! pk_data = PkData.create pk 
//                 return DcppReceiveMessage.Lock {
//                     lock = lock_data
//                     pk = pk_data 
//                 } 
//             | _ -> 
//                 return! Failure "couldnt parse"
//         }

//     let client = 
//         start_dcpp_client
//         |> HostnameData.fold <| connect_info.host
//         |> PortData.fold <| connect_info.port

//     let write dcpp_msg = 
//         match dcpp_msg with
//         | DcppSendMessage.MyPass mp_msg ->
//             client.write << Result.get << getBytes << PasswordData.fold (sprintf "$MyPass %s") <| mp_msg.password
//         | DcppSendMessage.ValidateNick vn_msg ->
//             let data = 
//                 Array.concat <| 
//                 [
//                     "$Key " |> getBytes |> Result.get |> (Array.collect byteToDCN);
//                     id |> KeyData.fold <| vn_msg.key;
//                     "|$ValidateNick " |> getBytes |> Result.get;
//                     Result.get << getBytes |> NickData.fold <| vn_msg.nick;
//                     "|" |> getBytes |> Result.get 
//                 ]

//             client.write data

//     let received = 
//         client.receivedEvent
//         |> Event.map (fun bytes ->
//             Result.success_workflow_with_string_failures {
//                 let! str = getString bytes
//                 return! parseDcppMessage str
//             })
//         |> Event.choose (fun r -> 
//             match r with
//             | Success msg -> Some msg
//             | _ -> None
//         )
    
//     { new ITransport with 
//         member __.Write(x) = write x
//         member __.Received = received
//         member __.Dispose() = client.dispose() }
//     |> Success

// [<EntryPoint>]
// let main argv =
//     let log = create_log()
//     try 
//         log.Info "FDCConsoleUI is starting! args: %A" argv
        
//         Result.success_workflow_with_string_failures {
//             let! host = HostnameData.create "localhost"
//             let! port = PortData.create 411

//             let connect_info = {
//                 host = host
//                 port = port 
//             }
//             let! nick = NickData.create "MnZrKk"
//             let pass_maybe = None

//             let res = 
//                 start_queue
//                 <| create_log
//                 <| create_transport
//                 <| (fun agent -> async { do! Async.Sleep 10000 })
//                 <| connect_info
//                 <| (nick, pass_maybe)

//             return! res
//         }
//         |>! Result.map (fun _ -> log.Info "Successfully finished main loop")
//         |>! Result.mapFailure (fun e -> log.Error "Error during main loop: %A" e)
//         |> ignore

//         log.Info "Shutting down..."
//     with ex ->
//         log.Error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
//         raise ex
//     0
