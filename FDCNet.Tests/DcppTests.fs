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
