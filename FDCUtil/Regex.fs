module FDCUtil.Regex

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// let applyRegexMatch str (r, f) =
//     match str with
//     | Regex r vars -> Some <| f vars
//     | _ -> None
