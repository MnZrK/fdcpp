module FDCConsoleUI.Infrastructure

open System
open System.Threading
open System.Reactive.Subjects
open System.Reactive.Concurrency
open FSharp.Control.Reactive
open FSharp.Configuration

open FDCUtil.Main
open FDCUtil.Main.Regex
open FDCDomain.MessageQueue
open FDCLogger

type IRawTransport = 
    inherit IDisposable 
    abstract Received: IConnectableObservable<byte[]>
    abstract Write: byte[] -> unit
    abstract Close: unit -> unit

module TcpStream = 
    type Error = 
    | CouldntConnect of string

    let private connect_async (client: System.Net.Sockets.TcpClient) (host: string) (port: int) = async {
        try 
            do! client.ConnectAsync(host, port) |> Async.AwaitTask
            return Success ()
        with 
        | :? AggregateException as ex ->
            (client :> IDisposable).Dispose()
            return 
                CouldntConnect <|
                    match ex.InnerException with
                    | :? System.Net.Sockets.SocketException as s_ex ->
                        s_ex.Message
                    | _ ->
                        ex.Message
                |> Failure
    }

    let start_async eom_marker (host: string) (port: int) = AsyncResult.success_workflow {
        let client = new System.Net.Sockets.TcpClient()

        do! connect_async client host port

        let! stream = async { 
            return 
                try 
                    client.GetStream() |> Success
                with
                | :? System.InvalidOperationException as ex when ex.Message = "The operation is not allowed on non-connected sockets." ->
                    (client :> IDisposable).Dispose()
                    CouldntConnect "Non-connected socket" |> Failure
                | _ ->
                    (client :> IDisposable).Dispose()
                    reraise()
        }

        let cts = new CancellationTokenSource()

        let observable = 
            read_message_seq 256 eom_marker stream
            |> Observable.ofSeqOn NewThreadScheduler.Default 
            |> Observable.publish 
        let dispose_observable = Observable.connect observable

        let write_agent = MailboxProcessor.Start((fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                // this will throw some exceptions when cancelled and disconnected, 
                //  but we don't care because they are silently swallowed
                //  and completion of Received observable will trigger anyways
                do! stream.WriteAsync(msg, 0, Array.length msg) |> Async.AwaitIAsyncResult |> Async.Ignore
                return! loop()
            } 
            loop()
        ), cts.Token)

        let dispose = 
            (fun () ->
                cts.Cancel()
                (cts :> IDisposable).Dispose() 
                client.Close() 
                (client :> IDisposable).Dispose()
                (write_agent :> IDisposable).Dispose()
                dispose_observable.Dispose()
            )
            |> callable_once

        return { new IRawTransport with
            member __.Received = observable
            member __.Write(x) = write_agent.Post x // it is safe to call Post on disposed MailboxProcessor
            member __.Close() = dispose()
            member __.Dispose() = dispose() }         
    }
        
let create_log() = (new Logger() :> ILogger)

let create_transport (connect_info: ConnectionInfo) =
    let log = create_log()

    let eom_marker = Convert.ToByte '|'
    
    TcpStream.start_async eom_marker 
    |> HostnameData.fold <| connect_info.host
    |> PortData.fold <| connect_info.port        

    |> Async.RunSynchronously
    |> Result.mapFailure (
        function
        | TcpStream.Error.CouldntConnect reason ->
            TransportError.CouldntConnect reason
    )
    |> Result.map (fun client ->
        let received =
            client.Received
            |> Observable.choose (fun msg ->
                let getStringF = getString >> Result.mapFailure (sprintf "Error: %A")
                let convertStringF = DCNstring_to_DcppMessage >> Result.mapFailure (fun (reason, input) -> 
                    sprintf "Error: \"%s\" for input: \"%s\"" reason input
                    )

                let res = 
                    msg
                    |> getStringF
                    // |>! Result.map (log.Debug "Raw received message: %s")
                    |> Result.bind <| convertStringF
                    |>! Result.mapFailure (log.Error "%s")
                    |> Result.toOption
                res
            )

        { new ITransport with 
            member __.Received = received
            member __.Dispose() = client.Dispose()
            member __.Write(msg) = 
                msg 
                |> DcppMessage_to_bytes 
                |>! (getString >> Result.map (log.Debug "Sending message: %s"))
                |> client.Write
        }
    )

module Settings = 
    type T = 
        { hub_connection_info: ConnectionInfo
        ; listen_info: ListenInfo
        ; nick: NickData.T
        ; pass_maybe: PasswordData.T option }

    type private RawSettings = AppSettings<"App.config">

    let create () = 
        Result.success_workflow_with_string_failures {
            let! hub_address = HostnameData.create RawSettings.HubAddress
            let! hub_port = PortData.create RawSettings.HubPort
            let hub_connection_info = 
                { host = hub_address
                ; port = hub_port }

            let! listen_address = IpAddress.create RawSettings.ListenAddress
            let! listen_port = PortData.create RawSettings.ListenPort
            let listen_info = 
                { ip = listen_address
                ; port = listen_port }

            let! nick = NickData.create RawSettings.Nickname
            let! pass_maybe = 
                if RawSettings.Password = String.Empty then Success None
                else PasswordData.create RawSettings.Password |> Result.map Some
            
            return 
                { hub_connection_info = hub_connection_info
                ; listen_info = listen_info
                ; nick = nick
                ; pass_maybe = pass_maybe }            
        }
