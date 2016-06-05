// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

// // Define your library scripting code here
// #I @"..\packages\NLog\lib\net40\"
// #r "NLog.dll"

// let loggerName = "logger"
// let logFileName = @"C:\Temp\myLogFile.txt"
// let fileTarget = 
//     let y = new NLog.Targets.FileTarget()
//     y.Name <- loggerName
//     y.FileName <- NLog.Layouts.Layout.op_Implicit( logFileName )
//     y

// let loggingConfig = 
//     let y = NLog.Config.LoggingConfiguration()
//     y.AddTarget(loggerName, fileTarget)
//     y.LoggingRules.Add( NLog.Config.LoggingRule( "*", NLog.LogLevel.Debug, fileTarget ) )
//     y

// NLog.LogManager.Configuration <- loggingConfig
// let logger = NLog.LogManager.GetLogger(loggerName);
// logger.Info("first log message.")
// logger.Factory.Flush()

// let rec asyncSendInput (stream : NetworkStream): Async<unit> =
//     async {
//         let input = Console.Read() |> BitConverter.GetBytes
//         input |> Array.iter stream.WriteByte
//         return! asyncSendInput stream
//     }

// let rec asyncPrintResponse (stream : NetworkStream): Async<unit> =
//     async {
//         let response = stream.ReadByte() |> Char.ConvertFromUtf32
//         Console.Write(response)
//         return! asyncPrintResponse stream
//     }

// let client = new System.Net.Sockets.TcpClient()
// client.Connect("localhost", 411)
// printfn "Connected to %A %A..." "localhost" 411
// let stream = client.GetStream()
// printfn "Got stream, starting two way asynchronous communication."
// asyncSendInput stream |> Async.Start
// asyncPrintResponse stream |> Async.RunSynchronously


