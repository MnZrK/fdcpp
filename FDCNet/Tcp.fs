module FDCNet.Tcp

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading

open FDCLogger

type Client = {
    /// function to close connection
    dispose: unit -> unit
    /// event for received messages
    receivedEvent: IEvent<byte[]>
    /// Write function to send replies. If connection is already closed, calling
    /// this function won't trigger any errors, but nothing will happen as well.
    write: byte[] -> unit
}

/// It assumes that the message is received when `checkMsgReceived` argument returns true.
let startClientAsync (checkMsgReceived: byte list * byte -> bool) (hostname: string) (port: int) =
    let logger = new Logger()
    let receivedCommand = new Event<byte[]>()

    let client = new System.Net.Sockets.TcpClient()
    async {
        // do! client.ConnectAsync(hostname, port) |> Async.AwaitIAsyncResult |> Async.Ignore
        // TODO fix async 
        logger.Info "Connecting to (%A %A) ..." hostname port
        client.Connect(hostname, port)
        logger.Info "Connected to %A %A" hostname port //TODO what if host is not available
        let stream = client.GetStream()
        
        // TODO use list or something instead of byte[]
        let rec asyncReadingLoop (message: byte list) (stream : NetworkStream) = async {
            let! bytes = stream.AsyncRead(1)
            if checkMsgReceived(message, bytes.[0]) then
                logger.Trace "Triggering `ReceivedCommand` event"
                receivedCommand.Trigger(List.toArray message)
                return! asyncReadingLoop [] stream
            else
                return! asyncReadingLoop (List.append message (List.ofArray bytes)) stream 
        }
        
        let cts = new CancellationTokenSource()
        Async.Start(asyncReadingLoop [] stream, cancellationToken = cts.Token)
        let dispose() =  
            logger.Info "Disposing..." 
            cts.Cancel() 
            client.Close() // TODO check if `Close` is happenning after all tasks related to cts are cancelled 
            
        // TODO make proper cancellation for agent 
        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec asyncWritingLoop() = async {
                let! msg = inbox.Receive()
                
                logger.Trace "Sending response..."
                do! stream.WriteAsync(msg, 0, Array.length msg, cts.Token) |> Async.AwaitIAsyncResult |> Async.Ignore
                
                return! asyncWritingLoop()
            }
            asyncWritingLoop()
        , cts.Token)
        
        return {dispose = dispose; receivedEvent = receivedCommand.Publish; write = agent.Post}
    }
