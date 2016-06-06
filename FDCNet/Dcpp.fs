module FDCNet.Dcpp

open System

let startClient = Tcp.startClient (fun (msg, byte) -> byte = Convert.ToByte '|')  