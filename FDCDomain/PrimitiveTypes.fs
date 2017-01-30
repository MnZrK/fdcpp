[<AutoOpen>]
module FDCDomain.PrimitiveTypes

open FDCUtil

module PkData =
    type T = PkData of string

    let create = mapNullString PkData

    let apply f (PkData s) = f s

module LockData =
    type T = LockData of ASCIIString.T

    let create (input: string) =
        input
        |> ASCIIString.create
        |> Result.bind <|
        (fun s ->
            let length = ASCIIString.apply String.length s
            if length < 2 then
                StringError.MustNotBeShorterThan 2 |> Failure
            else
                LockData s |> Success
        )
    let generate () =
        let rnd = System.Random()
        let len = rnd.Next(81, 134)
        ASCIIString.generate len |> LockData

    let apply f (LockData s) = f s
    let unwrap (LockData s) = s

module NickData =
    type T = NickData of ASCIIString.T

    let create (str: string) =
        let forbidden_chars = ['|'; '$'; ' ']

        let canonicalized = System.Text.RegularExpressions.Regex.Replace(str,"\s"," ").Trim()

        let forbidden = forbidden_chars |> List.tryFind (fun c -> canonicalized.IndexOf(string c) >= 0)

        match forbidden with
        | Some c -> StringError.IncludesForbiddenCharacter c |> Failure
        | None ->
            canonicalized
            |> ASCIIString.create
            |> Result.map NickData

    let apply f (NickData s) = f s
    let unwrap (NickData s) = s

module KeyData =
    type T = KeyData of byte[]

    let create (lockData: LockData.T) =
        let lockBytes = LockData.apply ASCIIString.getBytes lockData
        let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy)

        let lockLen = lockBytes.Length
        let key = Array.init lockLen (fun index ->
            if (index = 0) then
                lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
            else
                lockBytes.[index] ^^^ lockBytes.[index-1]
        )

        key |> Array.map nibbleSwap |> Array.collect byteToDCN |> KeyData

    let apply f (KeyData b) = f b
    let unwrap (KeyData b) = b

module PasswordData =
    type T = PasswordData of ASCIIString.T

    let create (str: string) =
        let forbidden_chars = ['|'; '$'; ' ']

        let canonicalized = System.Text.RegularExpressions.Regex.Replace(str,"\s"," ").Trim()

        let forbidden = forbidden_chars |> List.tryFind (fun c -> canonicalized.IndexOf(string c) >= 0)

        match forbidden with
        | Some c -> StringError.IncludesForbiddenCharacter c |> Failure
        | None ->
            canonicalized
            |> ASCIIString.create
            |> Result.map PasswordData

    let apply f (PasswordData p) = f p
    let unwrap (PasswordData p) = p

    let getBytes (PasswordData pass) =
        getBytes |> ASCIIString.apply <| pass
        |> Result.get
module HostnameData =
    type T = HostnameData of string

    let create = mapNullString HostnameData

    let apply f (HostnameData h) = f h

module PortData =
    type T = PortData of int

    type Error =
    | Negative
    | TooBig

    let create port =
        match port with
        | _ when port <= 0 -> Error.Negative |> Failure
        | _ when port >= 65535 -> Error.TooBig |> Failure
        | _ -> PortData port |> Success

    let apply f (PortData p) = f p
    let unwrap (PortData p) = p

module Direction = 
    type T =
    | Upload
    | Download

    let create str = 
        match str with
        | "Upload" -> Success Upload
        | "Download" -> Success Download
        | _ -> StringError.CouldntConvert (exn "doesn't match") |> Failure
