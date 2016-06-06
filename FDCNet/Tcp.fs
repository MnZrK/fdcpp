module FDCNet.Tcp

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading

open FDCLogger

/// return disposable object to close connection, event for received 
/// messages and post function to send replies. If connection is closed,
/// post function usage won't trigger any errors, but nothing will happen
let startClient (hostname: string) (port: int) =
    let logger = new Logger()
    let receivedCommand = new Event<byte[]>()

    let client = new System.Net.Sockets.TcpClient()
    client.Connect(hostname, port)
    logger.Info "Connected to %A %A" hostname port //TODO what if host is not available
    let stream = client.GetStream()
    
    // TODO use list or something instead of byte[]
    let rec asyncReadingLoop (message: byte[]) (stream : NetworkStream) = async {
        let! bytes = stream.AsyncRead(1)
        if (bytes.[0] = Convert.ToByte '|') then
            logger.Trace "Triggering `ReceivedCommand` event"
            receivedCommand.Trigger(message)
            return! asyncReadingLoop Array.empty stream
        else
            return! asyncReadingLoop (Array.append message bytes) stream 
    }
    
    let cts = new CancellationTokenSource()
    Async.Start(asyncReadingLoop Array.empty stream, cancellationToken = cts.Token)
    let disposable = { new IDisposable with member x.Dispose() = logger.Info "Disposing..."; cts.Cancel(); client.Close() } // TODO check if `Close` is happenning after all tasks are cancelled
        
    let agent = MailboxProcessor.Start(fun inbox -> 
        let rec asyncWritingLoop() = async {
            let! msg = inbox.Receive()
            
            logger.Trace "Sending response..."
            do! stream.WriteAsync(msg, 0, Array.length msg, cts.Token) |> Async.AwaitIAsyncResult |> Async.Ignore
            
            return! asyncWritingLoop()
        }
        asyncWritingLoop()
    , cts.Token)
    
    (disposable, receivedCommand.Publish, agent.Post)

// TODO Probably should remove it
type Socket with
    member socket.AsyncAccept() = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)
    member socket.AsyncReceive(buffer:byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length
        let beginReceive(b,o,c,cb,s) = socket.BeginReceive(b,o,c,SocketFlags.None,cb,s)
        Async.FromBeginEnd(buffer, offset, count, beginReceive, socket.EndReceive)
    member socket.AsyncSend(buffer:byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length
        let beginSend(b,o,c,cb,s) = socket.BeginSend(b,o,c,SocketFlags.None,cb,s)
        Async.FromBeginEnd(buffer, offset, count, beginSend, socket.EndSend)


/// My awesome server for testing purposes. Won't be used in actual program.
type Server() =
    static let logger = new Logger()

    static member Start(hostname:string, ?port) =
        let ipAddress = Dns.GetHostEntry(hostname).AddressList.[0]
        Server.Start(ipAddress, ?port = port)

    static member Start(?ipAddress, ?port) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        let port = defaultArg port 80
        let endpoint = IPEndPoint(ipAddress, port)
        let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)
        logger.Info "Started listening on port %d" port
        
        let rec loop() = async {
            logger.Trace "Waiting for request ..."
            let! socket = listener.AsyncAccept()
            logger.Trace "Received request"
            let response = [|
                "HTTP/1.1 200 OK\r\n"B
                "Content-Type: text/plain\r\n"B
                "\r\n"B
                "Hello World!"B |] |> Array.concat
            try
                try
                    let! bytesSent = socket.AsyncSend(response)
                    logger.Trace "Sent response"
                with e -> logger.ErrorException e "Error during sending response"
            finally
                logger.Trace "Closing socket..."
                socket.Shutdown(SocketShutdown.Both)
                socket.Close()
            return! loop() }

        let cts = new CancellationTokenSource()
        Async.Start(loop(), cancellationToken = cts.Token)
        { new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }
