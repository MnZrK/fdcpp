[<AutoOpen>]
module FDCDomain.Utilities

open FDCUtil

let getBytes (str: string) =
    try System.Text.Encoding.ASCII.GetBytes(str) |> Success
    with ex -> CouldntConvert ex |> Failure

let getByte (c: char) =
    try System.Convert.ToByte(c) |> Success
    with ex -> CouldntConvert ex |> Failure

let getString bytes =
    try System.Text.Encoding.ASCII.GetString(bytes) |> Success
    with ex -> CouldntConvert ex |> Failure

let byteToDCN b =
    let res =
        match b with
        | 0uy -> getBytes "/%DCN000%/"
        | 5uy -> getBytes "/%DCN005%/"
        | 36uy -> getBytes "/%DCN036%/"
        | 96uy -> getBytes "/%DCN096%/"
        | 124uy -> getBytes "/%DCN124%/"
        | 126uy -> getBytes "/%DCN126%/"
        | b -> [|b|] |> Success

    match res with
    | Success bytes -> bytes
    | _ -> failwith "It is impossible to get here, function always succeeds"

let bytesToDCN = Array.collect byteToDCN

let stringToDCN (s: string) =
    let getBytesL = getBytes >> Result.map List.ofArray
    let byteToDCNL = byteToDCN >> List.ofArray |> List.collect
    let getStringL = List.toArray >> getString

    s
    |> getBytesL
    |> Result.map byteToDCNL
    |> Result.bind <| getStringL

let DCNtoString (str: string) =
    let str0uy = getString [|0uy|] |> Result.get
    let str5uy = getString [|5uy|] |> Result.get
    let str36uy = getString [|36uy|] |> Result.get
    let str96uy = getString [|96uy|] |> Result.get
    let str124uy = getString [|124uy|] |> Result.get
    let str126uy = getString [|126uy|] |> Result.get

    let matches = [
        ("/%DCN000%/", str0uy);
        ("/%DCN005%/", str5uy);
        ("/%DCN036%/", str36uy);
        ("/%DCN096%/", str96uy);
        ("/%DCN124%/", str124uy);
        ("/%DCN126%/", str126uy)
    ]

    matches
    |> List.fold (fun (res: string) (strFrom, strTo) ->
        res.Replace(strFrom, strTo)
    ) str

let mapNullString f (s: string) =
    match s with
    | null -> StringError.Missing |> Failure
    | _ -> f s |> Success
