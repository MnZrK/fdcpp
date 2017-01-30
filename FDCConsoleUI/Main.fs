module FDCConcoleUI.Main

open System
open System.Threading

open FSharp.Configuration
open FSharp.Control.Reactive
open FSharpx.Control
open FSharpx.Control.Observable

open FDCDomain
open FDCConsoleUI.Infrastructure
open FDCUtil.Main

let linear_case (log: ILogger) (settings: Settings.T) search_res read_msg write_msg = 
    log.Trace "trying to read message synchronously"
    let (DcppReceiveMessage.MyNick mynick_msg) = read_msg() 
    log.Trace "rceived %A" mynick_msg
    log.Trace "trying to read message synchronously"
    let (DcppReceiveMessage.Lock lock_msg) = read_msg()

    log.Trace "sending nick"
    write_msg (DcppSendMessage.MyNick {
        MyNickMessage.nick = settings.nick // TODO get nick from state
    })

    log.Trace "sending lock"
    write_msg (DcppSendMessage.Lock {
        LockMessage.lock = LockData.generate()
        LockMessage.pk = PkData.create "abcabcabcabcabca" |> Result.get
    })
    
    log.Trace "sending supports"
    write_msg (DcppSendMessage.Supports)

    log.Trace "sending direction"
    write_msg (DcppSendMessage.Direction {
        direction = Direction.Download
        priority = System.Random().Next()
    })

    log.Trace "sending key"
    write_msg (DcppSendMessage.Key {
        key = KeyData.create lock_msg.lock
    })

    // let supports_msg = read_msg()
    // log.Info "got supports %A" supports_msg

    let (DcppReceiveMessage.Direction d_msg) = read_msg()
    log.Info "got message %A" d_msg
    
    log.Trace "sending get"
    write_msg (DcppSendMessage.Get {
        GetMessage.tth = search_res.tth
    })
    ()

let callback (log: ILogger) (settings: Settings.T) (agent: Agent) = 
    let wait_for_login = 
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
        |> Async.Ignore

    let eom_marker = Convert.ToByte '|'
    let som_marker_maybe = Some (Convert.ToByte '$')
    let listenip = IpAddress.unwrap settings.listen_info.ip
    let listenport = PortData.unwrap settings.listen_info.port
    let udpobs, disposable = Network.start_udpserver som_marker_maybe eom_marker listenport

    try
        udpobs
        |> Observable.map getString
        |> Observable.asUpdates
        |> Observable.add (log.Trace "Got message %A")

        let wait_for_search_result = 
            udpobs
            |> Observable.first
            |> Async.AwaitObservable

        use tcpserver = Network.start_tcpserver som_marker_maybe eom_marker listenport
        tcpserver.Accepted
        |> Observable.asUpdates
        |> Observable.add (log.Trace "Got message from tcp server %A")


        Async.RunSynchronously wait_for_login

        let search_str = "CopyWizEval.exe"
        log.Info "Posting search action for: %s" search_str
        agent.post << Send <| Search {
            listen_info = settings.listen_info
            search_str = search_str
        }

        let (SR search_res) = 
            wait_for_search_result 
            |> Async.RunSynchronously 
            |> getString 
            |> Result.get 
            |> DCNstring_to_DcppMessage
            |> Result.get
        log.Info "got search result %A" search_res

        tcpserver.Accepted
        |> Observable.add (fun transport ->
            let received_msgs =
                transport.Received
                |> Observable.map getString
                |> Observable.map (fun x -> Result.success_workflow_with_string_failures {
                    let! str = x
                    let! msg = DCNstring_to_DcppMessage str
                    return msg
                })
            
            let enum = Observable.getEnumerator received_msgs
            // use dispose_observer = 
            //     received_msgs 
            //     |> Observable.subscribe (log.Info "Got message from tcp socket %A")

            let read_msg() = enum.MoveNext() |> ignore; enum.Current |> Result.get
            let write_msg msg = transport.Write <| DcppMessage_to_bytes msg

            linear_case log settings search_res read_msg write_msg
            
            use dispose_temp = transport.Received |> Observable.subscribe (fun x -> log.Info "got message %A" x)
            ()
        )

        agent.post << Send <| ConnectToMe {
            listen_info = settings.listen_info
            remote_nick = search_res.nick_who_has_file
        }

        let need_exit = ref false
        Console.CancelKeyPress |> Event.add (fun x ->
            log.Warn "Got ctrl-c" 
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
            <| callback log settings
            <| settings.hub_connection_info
            <| (settings.nick, settings.pass_maybe)

        return! res
    }
    |>! Result.map (fun _ -> log.Info "Successfully finished main loop")
    |>! Result.mapFailure (fun e -> log.Error "Error during main loop: %A" e)
    |> ignore

    log.Info "Shutting down..."

    0
