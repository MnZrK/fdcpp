module FDCConsoleUI.Infrastructure

open System
open System.Threading
open System.Reactive.Subjects
open System.Reactive.Concurrency
open FSharp.Control.Reactive

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

    let private read_message_async eom_marker (stream: System.Net.Sockets.NetworkStream) = 
        let rec loop reversed_msg = async {
            // throws EndOfStreamException when connection is closed by remote side 
            //  (but it isn't thrown on this line for some reason)
            //  so have to catch it later
            let! bytes = stream.AsyncRead(1) 

            if bytes.[0] = eom_marker then
                return List.rev (bytes.[0]::reversed_msg) |> List.toArray
            else
                return! loop (bytes.[0]::reversed_msg)
        }
        async {
            try
                let! msg = loop []
                return Some msg
            with 
            | :? IO.EndOfStreamException -> // means the connection was closed
                return None 
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

        let read_message_seq =
            let read_message () = 
                try 
                    Async.RunSynchronously(read_message_async eom_marker stream, cancellationToken=cts.Token)
                with
                | :? OperationCanceledException ->
                    None
                | :? ObjectDisposedException -> // means WE closed the connection
                    None

            let rec loop () = seq {
                let msg_result = read_message()  
                match msg_result with
                | None -> ()
                | Some msg -> 
                    yield msg
                    yield! loop()
            }
            loop()

        let observable = 
            read_message_seq 
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
                    |>! Result.map (log.Debug "Raw received message: %s")
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

