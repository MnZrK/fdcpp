// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Main.fs"
open FDCNet

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