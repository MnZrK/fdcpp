module FDCConcoleUI.Main

open System
open System.Threading

open FSharp.Control.Reactive

open FDCDomain.MessageQueue
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

// [<EntryPoint>]
// let main argv =
//     printfn "Starting %A" Thread.CurrentThread.ManagedThreadId
//     let eom_marker = Convert.ToByte '|'
//     let client_result = TcpStreamClient.start_async eom_marker "localhost" 411 |> Async.RunSynchronously
//     client_result
//     |>! Result.mapFailure (fun e -> printfn "Error starting tcp client %A" e)
//     |> Result.mapSuccess (fun client -> using client (fun _ -> 
        
//         // Thread.Sleep 2000

//         printfn "Observable.add1 %A" Thread.CurrentThread.ManagedThreadId
//         use disposable = 
//             client.Received
//             // |>! Observable.add (fun x -> printfn "Observable.add %A %A" Thread.CurrentThread.ManagedThreadId x)
//             |> (Observable.subscribeWithCallbacks 
//                 <| (fun x -> printfn "onNext 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
//                 <| (fun x -> printfn "onError 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
//                 <| (fun x -> printfn "onCompleted 1 %A %A" Thread.CurrentThread.ManagedThreadId x)
//             ) 
//         printfn "finished setup 1 %A" Thread.CurrentThread.ManagedThreadId

//         // Thread.Sleep 2000
        
//         printfn "Observable.add2 %A" Thread.CurrentThread.ManagedThreadId
//         use anotherdisposable = 
//             client.Received
//                 |> (Observable.subscribeWithCallbacks 
//                     <| (fun x -> printfn "onNext 2 %A %A" Thread.CurrentThread.ManagedThreadId x)
//                     <| (fun x -> printfn "onError 2 %A %A" Thread.CurrentThread.ManagedThreadId x)
//                     <| (fun x -> printfn "onCompleted 2 %A %A" Thread.CurrentThread.ManagedThreadId x)

//                 )
//         printfn "finished setup 2 %A" Thread.CurrentThread.ManagedThreadId

//         Thread.Sleep 60000
//     ))
//     |> ignore
    
//     0

[<EntryPoint>]
let main argv =
    let log = create_log()
    log.Info "FDCConsoleUI is starting! args: %A" argv
        
    Result.success_workflow_with_string_failures {
        let! host = HostnameData.create "localhost"
        // let! host = HostnameData.create "p2p.academ.org"
        // let! host = HostnameData.create "peers.cn.ru"
        let! port = PortData.create 411

        let connect_info = {
            ConnectionInfo.host = host
            ConnectionInfo.port = port 
        }
        let listen_info = {
            ListenInfo.ip = IpAddress.create "127.0.0.1" |> Result.get
            ListenInfo.port = port 
        }
        let! nick = NickData.create "MnZrKk"
        let pass_maybe = PasswordData.create "dummypass" |> Result.toOption

        let res = 
            start_queue
            <| log
            <| create_transport
            <| (fun agent ->  
                // Thread.Sleep 5000

                // let search_str = "CopyWizEval.exe"

                // log.Info "Posting search action for: %s" search_str
                // agent.post << Send <| Search {
                //     listen_info = listen_info
                //     search_str = search_str
                // }


                // Thread.Sleep 55000

                // log.Info "Timedout, disconnecting"

                    // TODO fix it, doesnt look like it is working
                // Console.CancelKeyPress |> Event.add (fun x -> 
                //     x.Cancel <- true
                //     agent.post_and_reply Exit |> ignore)

                let rec loop() = 
                    Thread.Sleep 10000
                    loop()
                loop()
            )
            <| connect_info
            <| (nick, pass_maybe)

        return! res
    }
    |>! Result.map (fun _ -> log.Info "Successfully finished main loop")
    |>! Result.mapFailure (fun e -> log.Error "Error during main loop: %A" e)
    |> ignore

    log.Info "Shutting down..."

    0
