module FDCDomain.MessageQueue

open System

open FDCUtil.Main

// infrastructure interfaces
type ILogger =
    abstract Trace: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract TraceException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
    abstract Debug: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract DebugException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
    abstract Info: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract InfoException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
    abstract Warn: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract WarnException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
    abstract Error: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract ErrorException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
    abstract Fatal: fmt: Printf.StringFormat<'a, unit> -> unit
    abstract FatalException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> unit
type CreateLogger = unit -> ILogger 

// errors
type StringError = 
    | Missing
    | NotASCIIString
    | MustNotBeShorterThan of int
    | CouldntConvert of Exception

// utilities
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

// primitive types
module ASCIIString = 
    type T = ASCIIString of string

    let create =
        function
        | null -> StringError.Missing |> Failure
        | s -> 
            let isAscii = 
                String.forall 
                <| (fun c -> 
                        match getByte c with 
                        | Success b ->
                            126uy >= b && b >= 32uy
                        | _ -> 
                            false
                    ) 
                <| s 
            if isAscii then
                ASCIIString s |> Success
            else
                Failure StringError.NotASCIIString

    let fold f (ASCIIString s) = f s

    let getBytes (ASCIIString s) = getBytes s |> Result.get

module Pk = 
    type T = Pk of string

    let create = mapNullString Pk

    let fold f (Pk s) = f s 

module LockData =
    type T = LockData of ASCIIString.T

    let create (input: string) =
        ASCIIString.create input
        |> Result.bind <| 
        (fun s ->
            let length = ASCIIString.fold String.length s
            if length < 2 then
                StringError.MustNotBeShorterThan 2 |> Failure
            else 
                LockData s |> Success
        )

    let fold f (LockData s) = f s

module NickData = 
    type T = NickData of string

    let create = mapNullString NickData

    let fold f (NickData s) = f s

module KeyData = 
    type T = KeyData of byte[]

    let create (lockData: LockData.T) =
        let lockBytes = LockData.fold ASCIIString.getBytes lockData
        let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy) 

        let lockLen = lockBytes.Length
        let key = Array.init lockLen (fun index -> 
            if (index = 0) then
                lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
            else
                lockBytes.[index] ^^^ lockBytes.[index-1]
        ) 
        
        key |> Array.map nibbleSwap |> Array.collect byteToDCN

    let fold f (KeyData b) = f b

module PasswordData =
    type T = PasswordData of string

    let create = mapNullString PasswordData

    let fold f (PasswordData p) = f p

module HostnameData =
    type T = HostnameData of string

    let create = mapNullString HostnameData

    let fold f (HostnameData h) = f h

module PortData = 
    type T = PortData of int

    let create = PortData
    
    let fold f (PortData p) = f p

// domain models
type LockMessage = {
    lock: LockData.T
    pk: Pk.T
}

type HelloMessage = {
    nick: NickData.T
}

type ValidateNickMessage = {
    key: KeyData.T
    nick: NickData.T
}

type MyPassMessage = {
    password: PasswordData.T
}

type ConnectionInfo = {
    host: HostnameData.T
    port: PortData.T
}

// higher-order domain models
type DcppReceiveMessage = 
    | Lock of LockMessage
    | ValidateDenied 
    | GetPass
    | BadPass
    | Hello of HelloMessage
    | LoggedIn

type DcppSendMessage = 
    | ValidateNick of ValidateNickMessage
    | MyPass of MyPassMessage

type AgentAction =
    | SendMessage of DcppSendMessage
    | ReceiveMessage of DcppReceiveMessage
    | Connect of ConnectionInfo

let startQueue (create_log: CreateLogger) =
    let log = create_log()
    log.Trace "Starting queue..."
    
    ()