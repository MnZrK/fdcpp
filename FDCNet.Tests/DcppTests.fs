module FDCNet.Tests.Dcpp

open Xunit
open Swensen.Unquote

open FDCNet.Dcpp

module LockMessageTests = 

    [<Fact>]
    let ``Should parse correct hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
            
        let parsed = 
            match message with 
            | LockMessagePattern msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedLockMessage = {
            lock = "EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6"
            pk = "PtokaX"
        }
            
        let lockMessage = 
            match message with 
            | LockMessagePattern msg -> msg 
            | _ -> failwith "failed"
            
        test <@ lockMessage = expectedLockMessage @>
        
    [<Fact>]
    let ``Should extract correct values from correct hello message with DCN replacements`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tm/%DCN124%/g]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedLockMessage = {
            lock = "EXTENDEDPROTOCOLRxXB?79Tm|g]UXayOU7LYkSIq2Awg6"
            pk = "PtokaX"
        }
            
        let lockMessage = 
            match message with 
            | LockMessagePattern msg -> msg 
            | _ -> failwith "failed"
            
        test <@ lockMessage = expectedLockMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect hello message`` () =
        let message = "$Lockk EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pka=PtokaX"
            
        let parsed = 
            match message with 
            | LockMessagePattern msg -> true
            | _ -> false
            
        test <@ parsed = false @>

module ConvertLock2KeyTests =

    [<Fact>]
    let ``Should convert lock to key (simplest case)`` () =
        let lock = "1234"
        let expectedKey = [|51uy; 48uy; 16uy; 112uy|]
            
        let key = convertLockToKey (System.Text.Encoding.ASCII.GetBytes(lock))
            
        test <@ key = expectedKey @>

    [<Fact>]
    let ``Should convert lock to key (with DCN replacement)`` () =
        let lock = "1234asbasdf121\0||mam```341231"
        let expectedKey = [|99uy; 48uy; 16uy; 112uy; 85uy; 33uy; 17uy; 48uy; 33uy; 113uy; 32uy; 117uy;
        48uy; 48uy; 214uy; 198uy; 196uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy;
        48uy; 37uy; 47uy; 17uy; 192uy; 192uy; 208uy; 47uy; 37uy; 68uy; 67uy; 78uy;
        48uy; 48uy; 48uy; 37uy; 47uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy; 48uy;
        37uy; 47uy; 53uy; 112uy; 80uy; 48uy; 16uy; 32uy|]
        
        let key = convertLockToKey (System.Text.Encoding.ASCII.GetBytes(lock))
            
        test <@ key = expectedKey @>

    [<Fact>]
    let ``Should convert lock to key (and match sample from FlyLink)`` () =
        let parseHexKey (str: string) = str.Split [|' '|] |> Array.map (fun x -> System.Byte.Parse(x, System.Globalization.NumberStyles.AllowHexSpecifier))
    
        let lock = "EXTENDEDPROTOCOLSbWZ4Y^UXrsJBbhd=yxeVJlGdd8wg6"
        let expectedKey = parseHexKey "11 d1 c0 11 b0 a0 10 10 41 20 d1 b1 b1 c0 c0 30 f1 13 53 d0 e6 d6 70 b0 d0 a2 10 93 80 02 a0 c0 95 44 10 d1 33 c1 62 b2 32 2f 25 44 43 4e 30 30 30 25 2f c5 f4 01 15"
        
        let key = convertLockToKey (System.Text.Encoding.ASCII.GetBytes(lock))
            
        test <@ key = expectedKey @>
        