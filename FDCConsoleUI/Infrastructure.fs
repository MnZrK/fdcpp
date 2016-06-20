module FDCConsoleUI.Infrastructure

open System
open System.IO
open System.Net
open System.Net.Sockets

open System.Threading
open System.Threading.Tasks 
// open System.Reactive.Subjects
// open System.Reactive.Concurrency
open FSharp.Control.Reactive
open FSharpx.Control
open FSharpx.Control.Observable

open FSharp.Configuration

open FDCUtil
open FDCDomain.MessageQueue
open FDCLogger

// persistency layer
// this should have no dependencies on Domain
// TODO move it somewhere else
module Network = 
    type IRawTransport = 
        inherit IDisposable 
        abstract Received: IObservable<byte[]>
        abstract Write: byte[] -> unit

    type IServer = 
        inherit IDisposable
        abstract Accepted: IObservable<IRawTransport>

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
                    | :? SocketException as s_ex ->
                        s_ex.Message
                    | _ ->
                        ex.Message
                |> Failure
    }

    let start_client_async eom_marker (host: string) (port: int) = AsyncResult.success_workflow {
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

        let buffer_size = 256
        let subject = new Subject<_>()
        let start_reading () =
            try  
                for msg in concat_and_split_stream buffer_size eom_marker stream do
                    subject.OnNext(msg) 
            with
            | :? ObjectDisposedException
            | :? IOException ->
                subject.OnCompleted()
            | ex -> 
                subject.OnError(ex)
        Task.Factory.StartNew(
            start_reading, 
            cts.Token, 
            TaskCreationOptions.DenyChildAttach+TaskCreationOptions.LongRunning, 
            TaskScheduler.Default
        ) |> ignore

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
            fun () ->
                subject.OnCompleted() // it is safe to call onCompleted several times, the handler will be invoked only once
                cts.Cancel()
                (cts :> IDisposable).Dispose() 
                (stream :> IDisposable).Dispose()
                client.Close() 
                (client :> IDisposable).Dispose()
                (write_agent :> IDisposable).Dispose()
            |> callable_once

        return { new IRawTransport with
            member __.Received = Observable.asObservable subject
            member __.Write(x) = write_agent.Post x // it is safe to call Post on disposed MailboxProcessor
            member __.Dispose() = dispose() }         
    }

    let start_tcpserver eom_marker (host: string) (port: int) =
        printfn "Starting server..."
        // let ipaddress = Dns.GetHostEntry(host).AddressList.[0]
        let ipaddress = IPAddress.Loopback // TODO fix
        let endpoint = IPEndPoint(ipaddress, port)

        let cts = new CancellationTokenSource()


        let listener = new Socket(ipaddress.AddressFamily, SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp)
        
        printfn "Binding to %s:%d..." host port
        listener.Bind(endpoint)
        printfn "Listening..."
        listener.Listen(int SocketOptionName.MaxConnections)
        
        let subject = new Subject<IRawTransport>()

        let child_subjects = Observable.toList subject

        let loop_accept () = 
            let rec loop() = 
                printfn "Waiting for request ..."
                let socket = listener.Accept()
                printfn "Accepted request"

                let buffer_size = 256
                let obs = fetch_concat_split_from_socket buffer_size eom_marker socket cts.Token

                let write_agent = MailboxProcessor.Start((fun inbox -> 
                    let rec loop () = async {
                        let! msg = inbox.Receive()
                        // this will throw some exceptions when cancelled and disconnected, 
                        //  but we don't care because they are silently swallowed
                        //  and completion of Received observable will trigger anyways
                        do! socket.AsyncSend(msg, 0, Array.length msg) |> Async.Ignore
                        return! loop()
                    } 
                    loop()
                ), cts.Token)
                let dispose = 
                    (fun () ->
                        socket.Close()
                        (socket :> IDisposable).Dispose()
                        (write_agent :> IDisposable).Dispose()
                    )
                    |> callable_once

                let res = 
                    { new IRawTransport with
                        member __.Dispose() = dispose()
                        member __.Write(x) = write_agent.Post x
                        member __.Received = obs }

                loop()
            try loop()
            with ex -> subject.OnError(ex)

        printfn "Starting accept loop..."
        Task.Factory.StartNew(
            loop_accept, 
            cts.Token, 
            TaskCreationOptions.DenyChildAttach+TaskCreationOptions.LongRunning, 
            TaskScheduler.Default
        ) |> ignore

        let dispose = 
            (fun () ->
                subject.OnCompleted() 
                // BUG TODO fix memory leak!!! we are collecting ALL ever created sockets
                let res = 
                    child_subjects 
                    |> Async.AwaitObservable 
                    |> Async.RunSynchronously
                    |> Seq.map (fun t -> t.Dispose())
                cts.Cancel()
                (cts :> IDisposable).Dispose()
                listener.Close() // TODO will it dispose all accepted sockets as well ?  
                (listener :> IDisposable).Dispose()
            )
            |> callable_once

        { new IServer with
            member __.Accepted = Observable.asObservable subject
            member __.Dispose() = dispose() }

    let start_udpserver eom_marker (host: string) (port: int) =
        printfn "Starting udp server on %s:%d..." host port
        // let ipaddress = Dns.GetHostEntry(host).AddressList.[0]
        let ipaddress = IPAddress.Loopback
        let endpoint = IPEndPoint(ipaddress, port)

        let cts = new CancellationTokenSource()

        let receiver = new UdpClient(port)
        
        printfn "Created udp client %s:%d" host port
        let buffer_size = 256
        let obs = fetch_concat_split_from_socket buffer_size eom_marker receiver.Client cts.Token 

        let dispose = 
            (fun () ->
                // TODO call obs.OnCompleted ?
                cts.Cancel()
                (cts :> IDisposable).Dispose()
                receiver.Close() // TODO will it dispose all accepted sockets as well ?  
                (receiver :> IDisposable).Dispose()
            )
            |> callable_once

        obs, { new IDisposable with member __.Dispose() = dispose() }

// presentation layer
let create_log() = (new Logger() :> ILogger)

let create_transport (connect_info: ConnectionInfo) =
    let log = create_log()

    let eom_marker = Convert.ToByte '|'
    
    Network.start_client_async eom_marker 
    |> HostnameData.apply <| connect_info.host
    |> PortData.apply <| connect_info.port        

    |> Async.RunSynchronously
    |> Result.mapFailure (
        function
        | Network.Error.CouldntConnect reason ->
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

// presentation layer
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
