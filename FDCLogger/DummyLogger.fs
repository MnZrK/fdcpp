module FDCLogger.DummyLogger

open System
// TODO move ILogger somewhere else and update references
open FDCDomain.MessageQueue

type Logger() = 
    let dummy_log fmt = 
        Printf.kprintf ignore fmt

    member __.Trace fmt = dummy_log fmt
    member __.TraceException _ fmt = dummy_log fmt
    member __.Debug fmt = dummy_log fmt
    member __.DebugException _ fmt = dummy_log fmt
    member __.Info fmt = dummy_log fmt
    member __.InfoException _ fmt = dummy_log fmt
    member __.Warn fmt = dummy_log fmt
    member __.WarnException _ fmt = dummy_log fmt
    member __.Error fmt = dummy_log fmt
    member __.ErrorException _ fmt = dummy_log fmt
    member __.Fatal fmt = dummy_log fmt
    member __.FatalException _ fmt = dummy_log fmt 
    
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
