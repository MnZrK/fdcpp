module FDCConsoleUI.Infrastructure

open System
open System.Threading
open System.Reactive.Subjects
open FSharp.Control.Reactive

open FDCUtil.Main

module TcpStreamClient = 
    type T = 
        inherit IDisposable 
        abstract Received: IConnectableObservable<byte[]>
        abstract Write: byte[] -> unit

    let start_async eom_marker (host: string) (port: int) = async {
        let client = new System.Net.Sockets.TcpClient()
        do! client.ConnectAsync(host, port) |> Async.AwaitIAsyncResult |> Async.Ignore 
        let stream = client.GetStream()

        let read_message () = 
            let rec loop reversed_msg = async {
                let! bytes = stream.AsyncRead(1) // throws EndOfStreamException when connection is closed by remote side

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
                | :? IO.EndOfStreamException ->
                    return None 
            }

        let cts = new CancellationTokenSource()

        let read_message_seq =
            let sync_read_message () = 
                try 
                    Async.RunSynchronously(read_message(), cancellationToken=cts.Token)
                with
                | :? OperationCanceledException ->
                    None
                | :? ObjectDisposedException ->  
                    None

            let rec loop () = seq {
                let msg_result = sync_read_message()  
                match msg_result with
                | None -> ()
                | Some msg -> 
                    yield msg
                    yield! loop()
            }
            loop()

        let observable = Observable.ofSeqOn System.Reactive.Concurrency.NewThreadScheduler.Default read_message_seq |> Observable.publish |>! Observable.connect

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

        return 
            { new T with
                member __.Received = observable
                member __.Write(x) = write_agent.Post x
                member __.Dispose() = 
                    client.Close()
                    (client :> IDisposable).Dispose()
                    cts.Cancel() }
    }
        