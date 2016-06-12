module FDCDomain.MessageQueue.Tests

open Xunit
open Swensen.Unquote

open FDCUtil.Main
open FDCDomain.MessageQueue

module ``Utilities Tests`` = 

    [<Fact>]
    let ``Should get bytes from ASCII string`` () =
        test <@ getBytes "123" = Success [| 49uy; 50uy; 51uy |] @>
            
    [<Fact>]
    let ``Should get byte from ASCII char`` () =
        test <@ getByte '1' = Success 49uy @>

    [<Fact>]
    let ``Should get string from ASCII bytes`` () =
        test <@ getString [| 49uy; 50uy; 51uy |] = Success "123" @>
            
    [<Fact>]
    let ``Should convert dangerous characters to DCN`` () =
        test <@ 
                "12$3" 
                |> getBytes 
                |> Result.map (Array.collect byteToDCN) 
                |> Result.bind <| getString = Success "12/%DCN036%/3" 
            @>
        
    [<Fact>]
    let ``Should not touch input wihout dangerous characters`` () =
        let input = "123abbss"

        test <@ 
                input 
                |> getBytes 
                |> Result.map (Array.collect byteToDCN) 
                |> Result.bind <| getString = Success input 
            @>

    [<Fact>]
    let ``Should convert the same for bytes and strings`` () =
        let inputStr = "123abbss"
        let inputBytes = getBytes "123abbss"

        test <@ 
                inputBytes
                |> Result.map (Array.collect byteToDCN)
                |> Result.bind <| getString = stringToDCN inputStr 
            @>

    [<Fact>]
    let ``Should convert DCN string back to normal`` () =
        let input = "12/%DCN036%/3" 

        let expected = "12$3"

        test <@ DCNtoString input = expected @>

    [<Fact>]
    let ``Should convert DCN back and worth without change`` () =
        let input = "12$3|\01235ffffaa"

        test <@ "12$3|\01235ffffaa" |> stringToDCN |> Result.map DCNtoString = Success "12$3|\01235ffffaa" @>

module ``ASCIIString Tests`` =
    [<Fact>]
    let ``Should create ASCIIString from valid input`` () =
        let input = "12312351afdsf$$a  s^^%^%$#|dfqvz"

        test <@ ASCIIString.create input = Success (ASCIIString.ASCIIString input) @>

module ``LockData Tests`` =

    [<Fact>]
    let ``Should create lock from correct input`` () =
        let input = "12312351afdsfasdfqvz"

        test <@ LockData.create input = Success (LockData.LockData (ASCIIString.ASCIIString input)) @>

    [<Fact>]
    let ``Should not create lock from invalid input`` () =
        test <@ LockData.create null = Failure StringError.Missing @>
        test <@ LockData.create "" = Failure (StringError.MustNotBeShorterThan 2) @>
        test <@ LockData.create "a" = Failure (StringError.MustNotBeShorterThan 2) @>
        test <@ LockData.create "1" = Failure (StringError.MustNotBeShorterThan 2) @>

module ``KeyData Tests`` =

    [<Fact>]
    let ``Should create key from lock`` () =
        let parseHexKey (str: string) = str.Split [|' '|] |> Array.map (fun x -> System.Byte.Parse(x, System.Globalization.NumberStyles.AllowHexSpecifier))

        let input = [
            ("1234", [|51uy; 48uy; 16uy; 112uy|]);
            ("1234asbasdf121\0||mam```341231", [|99uy; 48uy; 16uy; 112uy; 85uy; 33uy; 17uy; 48uy; 33uy; 113uy; 32uy; 117uy;
        48uy; 48uy; 214uy; 198uy; 196uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy;
        48uy; 37uy; 47uy; 17uy; 192uy; 192uy; 208uy; 47uy; 37uy; 68uy; 67uy; 78uy;
        48uy; 48uy; 48uy; 37uy; 47uy; 47uy; 37uy; 68uy; 67uy; 78uy; 48uy; 48uy; 48uy;
        37uy; 47uy; 53uy; 112uy; 80uy; 48uy; 16uy; 32uy|]);
            ("EXTENDEDPROTOCOLSbWZ4Y^UXrsJBbhd=yxeVJlGdd8wg6", parseHexKey "11 d1 c0 11 b0 a0 10 10 41 20 d1 b1 b1 c0 c0 30 f1 13 53 d0 e6 d6 70 b0 d0 a2 10 93 80 02 a0 c0 95 44 10 d1 33 c1 62 b2 32 2f 25 44 43 4e 30 30 30 25 2f c5 f4 01 15")
        ]

        test 
            <@ 
                input 
                |> List.forall (fun (str, bytes) -> 
                    Result.mapSuccess KeyData.create (LockData.create str) = Success bytes
                )
            @>

module ``startQueue Tests`` =
    let dummy_logger =
        let dummy_log fmt = 
            Printf.kprintf ignore fmt

        { new ILogger with 
            member __.Trace fmt = dummy_log fmt
            member __.TraceException _ fmt = dummy_log fmt
            member __.Debug fmt = dummy_log fmt
            member __.DebugException _ fmt = dummy_log fmt
            member __.Info fmt = dummy_log fmt
            member __.InfoException _ fmt = dummy_log fmt
            member __.Warn fmt = dummy_log fmt
            member __.WarnException _ fmt = dummy_log fmt
            member __.Error fmt = dummy_log fmt
            member __.ErrorException _ fmt = dummy_log fmt
            member __.Fatal fmt = dummy_log fmt
            member __.FatalException _ fmt = dummy_log fmt } 

    let create_dummy_logger () = dummy_logger

    [<Fact>]
    let ``Should start queue`` () =
        let connect_info = {
            host = HostnameData.create "localhost" |> Result.get
            port = PortData.create 411 |> Result.get
        }

        let res = 
            start_queue
            <| (async { do! Async.Sleep 1 })
            <| create_dummy_logger
            <| connect_info
        
        test <@ Result.isSuccess res @>
