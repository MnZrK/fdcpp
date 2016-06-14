module FDCLogger.NLogLogger 

open System

// TODO move ILogger somewhere else and update references
open FDCDomain.MessageQueue

/// All initial initialization for the logger is done in the 
/// static constructor, ie when the object of the class is 
/// created for the first time. Static constructors are thread-safe
/// so this logger is thread-safe as well.
type Logger() = 
    // NLog configuration initialization
    static do 
        (
            let consoleTarget = 
                let y = new NLog.Targets.ColoredConsoleTarget()
                y.Name <- "ColoredConsole"
                y.Layout <- NLog.Layouts.SimpleLayout.FromString("${longdate}|${threadid}|${level:uppercase=true}|${logger}|${message}")
                y
            let loggingConfig = 
                let y = NLog.Config.LoggingConfiguration()
                y.AddTarget("ColoredConsole", consoleTarget)
                y.LoggingRules.Add( NLog.Config.LoggingRule("*", NLog.LogLevel.Trace, consoleTarget ) )
                y
            NLog.LogManager.Configuration <- loggingConfig
        ) 

    // getting NLog logger instance
    let callerType = 
        System.Diagnostics.StackTrace(1, false)
            .GetFrames().[0]
            .GetMethod()
            .DeclaringType
    let logger = NLog.LogManager.GetLogger(callerType.Name)

    member __.Trace fmt =
        Printf.kprintf (fun s -> logger.Trace(s)) fmt
    member __.TraceException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Trace(e, s)) fmt
    member __.Debug fmt = 
        Printf.kprintf (fun s -> logger.Debug(s)) fmt
    member __.DebugException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Debug(e, s)) fmt
    member __.Info fmt = 
        Printf.kprintf (fun s -> logger.Info(s)) fmt
    member __.InfoException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Info(e, s)) fmt
    member __.Warn fmt =
        Printf.kprintf (fun s -> logger.Warn(s)) fmt
    member __.WarnException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Warn(e, s)) fmt
    member __.Error fmt = 
        Printf.kprintf (fun s -> logger.Error(s)) fmt
    member __.ErrorException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Error(e, s)) fmt
    member __.Fatal fmt =
        Printf.kprintf (fun s -> logger.Fatal(s)) fmt
    member __.FatalException (e:Exception) fmt =
        Printf.kprintf (fun s -> logger.Fatal(e, s)) fmt
    
    interface ILogger with 
        member __.Trace fmt = __.Trace fmt
        member __.TraceException e fmt = __.TraceException e fmt
        member __.Debug fmt = __.Debug fmt
        member __.DebugException e fmt = __.DebugException e fmt
        member __.Info fmt = __.Info fmt
        member __.InfoException e fmt = __.InfoException e fmt
        member __.Warn fmt = __.Warn fmt
        member __.WarnException e fmt = __.WarnException e fmt
        member __.Error fmt = __.Error fmt
        member __.ErrorException e fmt = __.ErrorException e fmt
        member __.Fatal fmt = __.Fatal fmt
        member __.FatalException e fmt = __.FatalException e fmt
        