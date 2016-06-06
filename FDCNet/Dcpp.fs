module FDCNet.Dcpp

open System
open System.Text.RegularExpressions

// parsing
let byteToDCN = 
    function
    | 0uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN000%/")
    | 5uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN005%/")
    | 36uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN036%/")
    | 96uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN096%/")
    | 124uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN124%/")
    | 126uy -> System.Text.Encoding.ASCII.GetBytes("/%DCN126%/")
    | b -> [|b|]
let DCNtoString (str: string) = 
    str.Replace("/%DCN000%/", System.Text.Encoding.ASCII.GetString([|0uy|]))
        .Replace("/%DCN005%/", System.Text.Encoding.ASCII.GetString([|5uy|]))
        .Replace("/%DCN036%/", System.Text.Encoding.ASCII.GetString([|36uy|]))
        .Replace("/%DCN096%/", System.Text.Encoding.ASCII.GetString([|96uy|]))
        .Replace("/%DCN124%/", System.Text.Encoding.ASCII.GetString([|124uy|]))
        .Replace("/%DCN126%/", System.Text.Encoding.ASCII.GetString([|126uy|]))
        
type LockMessage = { 
    lock: string // ASCII string 
    pk: string // ASCII string
}

type Message =
| LockMessage of LockMessage 

let (|LockMessagePattern|_|) input =
    let m = Regex.Match(input,"\$Lock (.*) Pk=(.*)")
    if (m.Success) then Some { lock = DCNtoString m.Groups.[1].Value; pk = DCNtoString m.Groups.[2].Value } else None

let parseMessage =
    function
    | LockMessagePattern msg -> Some <| LockMessage msg  
    | _ -> None
    
// validating
type ValidateMessageResult =
| Success 
| Fail of string
    
let validateMessage = 
    function
    | LockMessage msg -> 
        if msg.lock.Length >= 2 then
            Success
        else
            Fail "lock is too short"
    
/// length of lock must be at least 2. It is ensured by `validateMessage`
let convertLockToKey (lock: byte[]): byte[] =
    let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy) 

    let lockLen = lock.Length
    let key = Array.init lockLen (fun index -> 
        if (index = 0) then
            lock.[0] ^^^ lock.[lockLen-1] ^^^ lock.[lockLen-2] ^^^ 5uy
        else
            lock.[index] ^^^ lock.[index-1]
    ) 
    
    key |> Array.map nibbleSwap |> Array.collect byteToDCN
    
let startClient = Tcp.startClient (fun (msg, byte) -> byte = Convert.ToByte '|')  