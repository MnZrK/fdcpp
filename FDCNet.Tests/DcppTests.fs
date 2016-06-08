module FDCNet.Tests.Dcpp

open Xunit
open Swensen.Unquote

open FDCUtil.Main
open FDCNet.Dcpp

module ``MessageLock Tests`` = 
    open Message.Lock

    [<Fact>]
    let ``Should parse correct Lock message`` () =
        let input = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct Lock message`` () =
        let input = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedMessage = {
            lock'NotValidated = "EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6"
            pk'NotValidated = "PtokaX"
        }
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should extract correct values from correct Lock message with DCN replacements`` () =
        let input = "$Lock EXTENDEDPROTOCOLRxXB?79Tm/%DCN124%/g]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedMessage = {
            lock'NotValidated = "EXTENDEDPROTOCOLRxXB?79Tm|g]UXayOU7LYkSIq2Awg6"
            pk'NotValidated = "PtokaX"
        }
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect Lock message`` () =
        let input = "$Lockk EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pka=PtokaX"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = false @>

    [<Fact>]
    let ``Should validate correct Lock message`` () =
        let expected = {
            lock = LockData.T "EXTENDEDPROTOCOLRxXB?79Tm/%DCN124%/g]UXayOU7LYkSIq2Awg6"
            pk = "PtokaX"
        } 

        let result = Result.successWorkflow {
            let! lockData = LockData.create "EXTENDEDPROTOCOLRxXB?79Tm/%DCN124%/g]UXayOU7LYkSIq2Awg6"
            let input = {
                lock'NotValidated = LockData.value lockData
                pk'NotValidated = "PtokaX"
            }

            return! validate input 
        }

        test <@ result = Success expected @>

    [<Fact>]
    let ``Should not validate incorrect Lock message`` () =
        let input = {
            lock'NotValidated = "g"
            pk'NotValidated = "PtokaX"
        }

        let result = validate input 

        test <@ result = Failure "lock is too short" @>

    // TODO move it when `calculateKey` is moved as well
    module ``calculateKey Tests`` =

        [<Fact>]
        let ``Should calculate key (simplest case)`` () =
            let expected = [|51uy; 48uy; 16uy; 112uy|]

            let result = Result.successWorkflow {
                let! lockData = LockData.create "1234"

                return calculateKey lockData
            }
                
            test <@ result = Success expected @>

        [<Fact>]
        let ``Should calculate key (with DCN replacement)`` () =
            let expected = [|99uy; 48uy; 16uy; 112uy; 85uy; 33uy; 17uy; 48uy; 33uy; 113uy; 32uy; 117uy;
            48uy; 48uy; 214uy; 198uy; 196uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy;
            48uy; 37uy; 47uy; 17uy; 192uy; 192uy; 208uy; 47uy; 37uy; 68uy; 67uy; 78uy;
            48uy; 48uy; 48uy; 37uy; 47uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy; 48uy;
            37uy; 47uy; 53uy; 112uy; 80uy; 48uy; 16uy; 32uy|]

            let result = Result.successWorkflow {
                let! lockData = LockData.create "1234asbasdf121\0||mam```341231"

                return calculateKey lockData
            }
            
            test <@ result = Success expected @>

        [<Fact>]
        let ``Should calculate key (and match sample from FlyLink)`` () =
            let parseHexKey (str: string) = str.Split [|' '|] |> Array.map (fun x -> System.Byte.Parse(x, System.Globalization.NumberStyles.AllowHexSpecifier))
        
            let expected = parseHexKey "11 d1 c0 11 b0 a0 10 10 41 20 d1 b1 b1 c0 c0 30 f1 13 53 d0 e6 d6 70 b0 d0 a2 10 93 80 02 a0 c0 95 44 10 d1 33 c1 62 b2 32 2f 25 44 43 4e 30 30 30 25 2f c5 f4 01 15"

            let result = Result.successWorkflow {
                let! lockData = LockData.create "EXTENDEDPROTOCOLSbWZ4Y^UXrsJBbhd=yxeVJlGdd8wg6"

                return calculateKey lockData
            }
            
            test <@ result = Success expected @>

module ``MessageValidateDenied Tests`` = 
    open Message.ValidateDenied

    [<Fact>]
    let ``Should parse correct ValidateDenide message`` () =
        let input = "$ValidateDenide"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct ValidateDenide message`` () =
        let input = "$ValidateDenide"
        let expectedMessage = NotValidated ValidateDenied
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect ValidateDenide message`` () =
        let input = "$ValidateDenide1"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = false @>

    [<Fact>]
    let ``Should validate correct ValidateDenide message`` () =
        let input = NotValidated ValidateDenied

        let expected = ValidateDenied

        let result = validate input

        test <@ result = Success expected @>

module ``MessageGetPass Tests`` = 
    open Message.GetPass

    [<Fact>]
    let ``Should parse correct GetPass message`` () =
        let input = "$GetPass"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct GetPass message`` () =
        let input = "$GetPass"
        let expectedMessage = NotValidated GetPass
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect GetPass message`` () =
        let input = "$GetPass1"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = false @>
        
    [<Fact>]
    let ``Should validate correct GetPass message`` () =
        let input = NotValidated GetPass

        let expected = GetPass

        let result = validate input

        test <@ result = Success expected @>

module ``MessageBadPass Tests`` =
    open Message.BadPass

    [<Fact>]
    let ``Should parse correct BadPass message`` () =
        let input = "$BadPass"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct BadPass message`` () =
        let input = "$BadPass"
        let expectedMessage = NotValidated BadPass
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect BadPass message`` () =
        let input = "$BadPass1"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = false @>        
        
    [<Fact>]
    let ``Should validate correct BadPass message`` () =
        let input = NotValidated BadPass

        let expected = BadPass

        let result = validate input

        test <@ result = Success expected @>

module ``MessageHello Tests`` =
    open Message.Hello

    [<Fact>]
    let ``Should parse correct Hello message`` () =
        let input = "$Hello MnZrKk"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct Hello message`` () =
        let input = "$Hello MnZrKk"
        let expectedMessage = NotValidated {
            nick = NickData "MnZrKk" 
        }
            
        let message = 
            match input with 
            | RegexMsg msg -> msg 
            | _ -> failwith "failed"
            
        test <@ message = expectedMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect Hello message`` () =
        let input = "$Hello1 Mnzasdfasdf"
            
        let parsed = 
            match input with 
            | RegexMsg msg -> true
            | _ -> false
            
        test <@ parsed = false @>        
        
    [<Fact>]
    let ``Should validate correct Hello message`` () =
        let input = NotValidated {
            nick = NickData "MnZrKk" 
        }

        let expected = {
            nick = NickData "MnZrKk" 
        }

        let result = validate input

        test <@ result = Success expected @>
