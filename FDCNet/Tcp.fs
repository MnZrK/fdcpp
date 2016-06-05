namespace FDCNet

module Tcp =
    open System
    open System.IO
    open System.Net
    open System.Net.Sockets
    open System.Threading

    open FDCLogger

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

    type Server() =
        static let logger = new Logger()
    
        static member Start(hostname:string, ?port) =
            let ipAddress = Dns.GetHostEntry(hostname).AddressList.[0]
            Server.Start(ipAddress, ?port = port)

        static member Start(?ipAddress, ?port) =
            let ipAddress = defaultArg ipAddress IPAddress.Any
            let port = defaultArg port 80
            let endpoint = IPEndPoint(ipAddress, port)
            let cts = new CancellationTokenSource()
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

            Async.Start(loop(), cancellationToken = cts.Token)
            { new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }
