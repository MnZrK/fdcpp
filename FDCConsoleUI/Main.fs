module FDCConcoleUI.Main

open System
open System.Threading

open FSharp.Configuration
open FSharp.Control.Reactive

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
            <| settings.hub_connection_info
            <| (settings.nick, settings.pass_maybe)

        return! res
    }
    |>! Result.map (fun _ -> log.Info "Successfully finished main loop")
    |>! Result.mapFailure (fun e -> log.Error "Error during main loop: %A" e)
    |> ignore

    log.Info "Shutting down..."

    0
