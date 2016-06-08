module FDCNet.Util

let getBytes (str: string) = System.Text.Encoding.ASCII.GetBytes(str)
let getString bytes = System.Text.Encoding.ASCII.GetString(bytes)

let byteToDCN = 
    function
    | 0uy -> getBytes "/%DCN000%/"
    | 5uy -> getBytes "/%DCN005%/"
    | 36uy -> getBytes "/%DCN036%/"
    | 96uy -> getBytes "/%DCN096%/"
    | 124uy -> getBytes "/%DCN124%/"
    | 126uy -> getBytes "/%DCN126%/"
    | b -> [|b|]
let stringToDCN = getBytes >> Array.collect byteToDCN >> getString
let DCNtoString (str: string) = 
    str.Replace("/%DCN000%/", getString [|0uy|])
        .Replace("/%DCN005%/", getString [|5uy|])
        .Replace("/%DCN036%/", getString [|36uy|])
        .Replace("/%DCN096%/", getString [|96uy|])
        .Replace("/%DCN124%/", getString [|124uy|])
        .Replace("/%DCN126%/", getString [|126uy|])
