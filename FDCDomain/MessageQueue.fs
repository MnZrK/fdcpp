module FDCDomain.MessageQueue

open FDCUtil.Main

// utilities
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

// errors
type StringError = 
    | Missing
    | MustNotBeShorterThan of int

// utilities'
let mapNullString f (s: string) = 
    match s with
    | null -> StringError.Missing |> Failure
    | _ -> f s |> Success

// primitive types
module Pk = 
    type T = Pk of string

    let create = mapNullString Pk

    let apply f (Pk s) = f s 

module LockData =
    type T = LockData of string

    let create (s: string) =
        match s with
        | null -> 
            StringError.Missing |> Failure
        | _ when s.Length < 2 -> 
            StringError.MustNotBeShorterThan 2 |> Failure 
        | _ -> 
            LockData s |> Success

    let apply f (LockData s) = f s

module NickData = 
    type T = NickData of string

    let create = mapNullString NickData

    let apply f (NickData s) = f s

module KeyData = 
    type T = KeyData of byte[]

    let create (lockData: LockData.T) =
        let lockBytes = LockData.apply getBytes lockData
        let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy) 

        let lockLen = lockBytes.Length
        let key = Array.init lockLen (fun index -> 
            if (index = 0) then
                lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
            else
                lockBytes.[index] ^^^ lockBytes.[index-1]
        ) 
        
        key |> Array.map nibbleSwap |> Array.collect byteToDCN

    let apply f (KeyData b) = f b

module PasswordData =
    type T = PasswordData of string

    let create = mapNullString PasswordData

    let apply f (PasswordData p) = f p

module HostnameData =
    type T = HostnameData of string

    let create = mapNullString HostnameData

    let apply f (HostnameData h) = f h

module PortData = 
    type T = PortData of int

    let create = PortData
    
    let apply f (PortData p) = f p

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
