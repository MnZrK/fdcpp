[<AutoOpen>]
module FDCUtil.Text

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    match input with
    | null -> 
        None
    | _ ->
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
