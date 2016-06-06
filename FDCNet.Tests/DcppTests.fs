module FDCNet.Tests.Dcpp

open Xunit
open Swensen.Unquote

open FDCNet.Dcpp

module HelloMessageTests = 

    [<Fact>]
    let ``Should parse correct hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
            
        let parsed = 
            match message with 
            | HelloMessagePattern msg -> true
            | _ -> false
            
        test <@ parsed = true @>
        
    [<Fact>]
    let ``Should extract correct values from correct hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedHelloMessage = {
            lock = "EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6"
            pk = "PtokaX"
        }
            
        let helloMessage = 
            match message with 
            | HelloMessagePattern msg -> msg 
            | _ -> failwith "failed"
            
        test <@ helloMessage = expectedHelloMessage @>
        
    [<Fact>]
    let ``Should not parse incorrect hello message`` () =
        let message = "$Lockk EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pka=PtokaX"
            
        let parsed = 
            match message with 
            | HelloMessagePattern msg -> true
            | _ -> false
            
        test <@ parsed = false @>
        
module DcppMessageTests =

    [<Fact>]
    let ``Should parse correct DCPP hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
            
        let parsed = 
            match parseMessage message with 
            | Some (HelloMessage msg) -> true
            | _ -> false 
            
        test <@ parsed = true @>

    [<Fact>]
    let ``Should extract correct values from correct DCPP hello message`` () =
        let message = "$Lock EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pk=PtokaX"
        let expectedHelloMessage = Some <| HelloMessage {
            lock = "EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6"
            pk = "PtokaX"
        }
            
        let helloMessage = parseMessage message
            
        test <@ helloMessage = expectedHelloMessage @>

    [<Fact>]
    let ``Should not parse incorrect DCPP hello message`` () =
        let message = "$Lockk EXTENDEDPROTOCOLRxXB?79Tmrg]UXayOU7LYkSIq2Awg6 Pka=PtokaX"
            
        let helloMessage = parseMessage message
            
        test <@ helloMessage = None @>