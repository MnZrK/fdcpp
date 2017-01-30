[<AutoOpen>]
module FDCDomain.BasePrimitiveTypes

open System

open FDCUtil

module ASCIIString =
    type T = ASCIIString of string

    let isAsciiByte b = 
        126uy >= b && b >= 32uy

    let create =
        function
        | null -> StringError.Missing |> Failure
        | s ->
            let isAscii =
                String.forall
                <| (fun c ->
                        match getByte c with
                        | Success b ->
                            isAsciiByte b
                        | _ ->
                            false
                    )
                <| s
            if isAscii then
                ASCIIString s |> Success
            else
                Failure StringError.NotASCIIString

    let generate len =
        let rnd = System.Random()
        let res = 
            Array.init len (fun _ -> byte (rnd.Next(32, 127)))
            |> getString 
            |> Result.get
            |> ASCIIString
        res

    let apply f (ASCIIString s) = f s
    let unwrap (ASCIIString s) = s

    let getBytes (ASCIIString s) = getBytes s |> Result.get

module IpAddress =
    type T = IpAddress of string

    let create (s: string) =
        try
            System.Net.IPAddress.Parse(s) |> Success
        with e ->
            StringError.CouldntConvert e |> Failure
        |> Result.map (fun ip ->
            ip.ToString() |> IpAddress
        )
    let apply f (IpAddress ip) = f ip
    let unwrap (IpAddress ip) = ip

    let getBytes (IpAddress ip) = getBytes ip |> Result.get

module PositiveInt =
    /// 0 is inclusive
    type T = PositiveInt of uint64

    type Error =
    | NegativeValue

    let create (str: string) =
        try
            Convert.ToUInt64(str) |> PositiveInt |> Success
        with ex ->
            StringError.CouldntConvert ex |> Failure

    let fromULong (i: uint64) = PositiveInt i

    let apply f (PositiveInt i) = f i
    let unwrap (PositiveInt i) = i
