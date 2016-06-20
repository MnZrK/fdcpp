module FDCConcoleUI.Main

open System
open System.Threading

open FSharp.Configuration
open FSharp.Control.Reactive
open FSharpx.Control
open FSharpx.Control.Observable

open FDCDomain.MessageQueue
open FDCConsoleUI.Infrastructure
open FDCUtil.Main

[<EntryPoint>]
let main argv =
    let log = create_log()
    log.Info "FDCConsoleUI is starting! args: %A" argv
        
    Result.success_workflow_with_string_failures {
        let! settings = Settings.create()

        let res = 
            start_queue
            <| log
            <| create_transport
            <| (fun agent ->
                let eom_marker = Convert.ToByte '|'
                let listenip = IpAddress.unwrap settings.listen_info.ip
                let listenport = PortData.unwrap settings.listen_info.port
                let udpobs, disposable = Network.start_udpserver eom_marker listenip listenport

                try 
                    udpobs
                    |> Observable.map getString
                    |> Observable.asUpdates
                    |> Observable.add (log.Trace "Got message %A")


                    agent.state_changed
                    |> Observable.filter (fun ((state, _), (state', _)) ->
                        match state, state' with
                        | _, LoggedIn env ->
                            true
                        | _, _ ->
                            false
                    )
                    |> Observable.first
                    |> Async.AwaitObservable
                    |> Async.RunSynchronously
                    |> ignore

                    let search_str = "CopyWizEval.exe"
                    log.Info "Posting search action for: %s" search_str
                    agent.post << Send <| Search {
                        listen_info = settings.listen_info
                        search_str = search_str
                    }

                    let need_exit = ref false
                    Console.CancelKeyPress |> Event.add (fun x ->
                        log.Info "Got ctrl-c" 
                        x.Cancel <- true
                        need_exit := true
                    )
                    let rec loop() = 
                        Thread.Sleep 500
                        if !need_exit then ()
                        else loop()
                    loop()
                finally
                    ()
                    disposable.Dispose()
            )
            <| settings.hub_connection_info
            <| (settings.nick, settings.pass_maybe)

        return! res
    }
    |>! Result.map (fun _ -> log.Info "Successfully finished main loop")
    |>! Result.mapFailure (fun e -> log.Error "Error during main loop: %A" e)
    |> ignore

    log.Info "Shutting down..."

    0
