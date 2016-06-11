module FDCUtil.Tests.UnquoteExtensions

#nowarn "44" // to access Swensen.Unquote.Assertions.Internal

open Swensen.Unquote

[<AutoOpen>]
module Assertions =
    let testO (expr:Microsoft.FSharp.Quotations.Expr<bool>) =
        try test expr; None 
        with ex -> Some ex

let inline tryRaise res =
    match res with
    | Some ex -> raise ex
    | None -> ()
