module FDCNet.Dcpp

open System
open System.Text.RegularExpressions

type HelloMessage = { lock: string; pk: string }

type Message =
| HelloMessage of HelloMessage 

let (|HelloMessagePattern|_|) input =
   let m = Regex.Match(input,"\$Lock (.*) Pk=(.*)")
   if (m.Success) then Some { lock = m.Groups.[1].Value; pk = m.Groups.[2].Value } else None

let parseMessage =
    function
    | HelloMessagePattern msg -> Some <| HelloMessage msg  
    | _ -> None
    
let startClient = Tcp.startClient (fun (msg, byte) -> byte = Convert.ToByte '|')  