module FDCLogger.ConsoleLogger

open System
// TODO move ILogger somewhere else and update references
open FDCDomain

type Logger() = 
    let logEvent prefix fmt =
        Printf.kprintf (fun s -> printfn "%s: %s" prefix s) fmt
    let logEventException prefix (e:Exception) fmt =
        Printf.kprintf (fun s -> printfn "%s: Exception %s: %s" prefix e.Message s) fmt
    
    member __.Trace fmt = logEvent "TRACE" fmt
    member __.TraceException e fmt = logEventException "TRACE" e fmt
    member __.Debug fmt = logEvent "DEBUG" fmt
    member __.DebugException e fmt = logEventException "DEBUG" e fmt
    member __.Info fmt = logEvent "INFO" fmt
    member __.InfoException e fmt = logEventException "INFO" e fmt
    member __.Warn fmt = logEvent "WARN" fmt
    member __.WarnException e fmt = logEventException "WARN" e fmt
    member __.Error fmt = logEvent "ERROR" fmt
    member __.ErrorException e fmt = logEventException "ERROR" e fmt
    member __.Fatal fmt = logEvent "FATAL" fmt
    member __.FatalException e fmt = logEventException "FATAL" e fmt
    
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
