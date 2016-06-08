module FDCNet.Dcpp

open System

open FDCUtil.Main
open FDCUtil.Regex
open FDCNet.Util

module Message =
    module Lock =
        module LockData =
            type T = T of string

            let create (str: string) = 
                if str.Length >= 2 then
                    T str |> Success  
                else
                    "lock is too short" |> Failure
            let value (T str) = str

        type T = { 
            lock: LockData.T // ASCII string 
            pk: string // ASCII string
        }
        type T'NotValidated = { 
            lock'NotValidated: string // ASCII string 
            pk'NotValidated: string // ASCII string
        }        
        let (|RegexMsg|_|) input =
            match input with
            | Regex "^\$Lock (.*) Pk=(.*)$" [ lock; pk ] -> 
                Some { 
                    lock'NotValidated = DCNtoString lock
                    pk'NotValidated = DCNtoString pk 
                } 
            | _ -> 
                None
        let parse = (|RegexMsg|_|)

        let validate msg = 
            LockData.create msg.lock'NotValidated |> Result.mapSuccess (fun lockData -> 
            {
                lock = lockData
                pk = msg.pk'NotValidated
            })

        // TODO probably move it somewhere else - It is about key, not about lock. We don't 
        //  really care if key is calculated only using lock or something else
        let calculateKey (LockData.T lock): byte[] =
            let lockBytes = getBytes lock
            let nibbleSwap b = ((b<<<4) &&& 240uy) ||| ((b>>>4) &&& 15uy) 

            let lockLen = lockBytes.Length
            let key = Array.init lockLen (fun index -> 
                if (index = 0) then
                    lockBytes.[0] ^^^ lockBytes.[lockLen-1] ^^^ lockBytes.[lockLen-2] ^^^ 5uy
                else
                    lockBytes.[index] ^^^ lockBytes.[index-1]
            ) 
            
            key |> Array.map nibbleSwap |> Array.collect byteToDCN
                

    module ValidateDenied =
        type T = ValidateDenied
        type T'NotValidated = NotValidated of T
        let (|RegexMsg|_|) input =
            match input with
            | Regex "^\$ValidateDenide$" [] -> 
                NotValidated ValidateDenied |> Some
            | _ -> 
                None 
        let parse = (|RegexMsg|_|)
        let validate (NotValidated msg) = Success msg

    module GetPass =
        type T = GetPass
        type T'NotValidated = NotValidated of T
        let (|RegexMsg|_|) input =
            match input with
            | Regex "^\$GetPass$" [] -> 
                NotValidated GetPass |> Some
            | _ -> 
                None 
        let parse = (|RegexMsg|_|)
        let validate (NotValidated msg) = Success msg

    module BadPass =
        type T = BadPass
        type T'NotValidated = NotValidated of T
        let (|RegexMsg|_|) input =
            match input with
            | Regex "^\$BadPass$" [] -> 
                NotValidated BadPass |> Some
            | _ -> 
                None 
        let parse = (|RegexMsg|_|)
        let validate (NotValidated msg) = Success msg

    module Hello =
        type NickData = NickData of string
        type T = { nick: NickData }
        type T'NotValidated = NotValidated of T
        let (|RegexMsg|_|) input =
            match input with
            | Regex "^\$Hello (.*)$" [ nick ] -> 
                NotValidated { nick = NickData nick } |> Some
            | _ -> 
                None 
        let parse = (|RegexMsg|_|)
        let validate (NotValidated msg) = Success msg

    type T =
    | LockT           of Lock.T
    | ValidateDeniedT of ValidateDenied.T
    | GetPassT        of GetPass.T
    | BadPassT        of BadPass.T
    | HelloT          of Hello.T

    type T'NotValidated =
    | LockT'NotValidated           of Lock.T'NotValidated
    | ValidateDeniedT'NotValidated of ValidateDenied.T'NotValidated
    | GetPassT'NotValidated        of GetPass.T'NotValidated
    | BadPassT'NotValidated        of BadPass.T'NotValidated
    | HelloT'NotValidated          of Hello.T'NotValidated

    let (|RegexMsg|_|) input =
        match input with
        | Lock.RegexMsg msg           -> LockT'NotValidated msg           |> Some
        | ValidateDenied.RegexMsg msg -> ValidateDeniedT'NotValidated msg |> Some 
        | GetPass.RegexMsg msg        -> GetPassT'NotValidated msg        |> Some
        | BadPass.RegexMsg msg        -> BadPassT'NotValidated msg        |> Some
        | Hello.RegexMsg msg          -> HelloT'NotValidated msg          |> Some
        | _ -> None

    let parse = (|RegexMsg|_|)

    let validate msg =
        match msg with
        | LockT'NotValidated msg           -> Lock.validate msg           |> Result.mapSuccess LockT
        | ValidateDeniedT'NotValidated msg -> ValidateDenied.validate msg |> Result.mapSuccess ValidateDeniedT
        | GetPassT'NotValidated msg        -> GetPass.validate msg        |> Result.mapSuccess GetPassT
        | BadPassT'NotValidated msg        -> BadPass.validate msg        |> Result.mapSuccess BadPassT
        | HelloT'NotValidated msg          -> Hello.validate msg          |> Result.mapSuccess HelloT

let startClientAsync = Tcp.startClientAsync (fun (msg, byte) -> byte = Convert.ToByte '|')
