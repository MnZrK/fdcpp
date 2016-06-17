module FDCDomain.MessageQueue.Tests

open System

open Xunit
open Swensen.Unquote

open FDCUtil.Main
open FDCUtil.Main.Regex
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
                    Result.mapSuccess KeyData.create (LockData.create str) = Success (KeyData.KeyData bytes)
                )
            @>

module ``startQueue Tests`` =
    // [<Fact>]
    // let ``Should trigger IObservable from IEvent`` () =
    //     let event = new Event<int>()

    //     let create_observable ievent: IObservable<int> = ievent

    //     let observable: IObservable<int> = create_observable event.Publish 
    //     observable |> Observable.add (fun x -> printfn "Got %A" x)

    //     event.Trigger(10)

    //     test <@ true @>

    // [<Fact>]
    // let ``Should scan IObservable from IEvent`` () =
    //     let event = new Event<int>()

    //     let create_observable ievent: IObservable<int> = ievent

    //     let observable: IObservable<int> = create_observable event.Publish 
    //     observable |> Observable.scan (fun state x -> printfn "Got %A" x; state) 10 |> Observable.add ignore

    //     event.Trigger(10)

    //     test <@ false @>

    let timeout = 500

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

    let console_logger = 
        let logEvent prefix fmt =
            Printf.kprintf (fun s -> printfn "%s: %s" prefix s) fmt
        let logEventException prefix (e:System.Exception) fmt =
            Printf.kprintf (fun s -> printfn "%s: Exception %s: %s" prefix e.Message s) fmt
    
        { new ILogger with
            member __.Trace fmt = logEvent "TRACE" fmt
            member __.TraceException e fmt = logEventException "TRACE" e fmt
            member __.Debug fmt = logEvent "DEBUG" fmt
            member __.DebugException e fmt = logEventException "DEBUG" e fmt
            member __.Info fmt = logEvent "INFO" fmt
            member __.InfoException e fmt = logEventException "INFO" e fmt
            member __.Warn fmt = logEvent "WARN" fmt
            member __.WarnException e fmt = logEventException "WARN" e fmt
            member __.Error fmt = logEvent "ERROR" fmt
            member __.ErrorException e fmt = logEventException "ERROR" e fmt
            member __.Fatal fmt = logEvent "FATAL" fmt
            member __.FatalException e fmt = logEventException "FATAL" e fmt }

    let create_dummy_logger () = dummy_logger

    [<Fact>]
    let ``Should start queue and pass authentication`` () =
        let create_dummy_transport_has_been_called = ref false
        let write_has_been_called = ref false
        let create_dummy_transport ievent connect_info =
            create_dummy_transport_has_been_called := true
            { new ITransport with
                member __.Received = ievent
                member __.Write (x) = write_has_been_called := true
                member __.Dispose() = () } 
            |> Success

        let event = new Event<DcppReceiveMessage>() 
    
        let connect_info = {
            host = HostnameData.create "localhost" |> Result.get
            port = PortData.create 411 |> Result.get
        }
        let nick_data = NickData.create "MnZrKk" |> Result.get

        let res = 
            start_queue
            <| create_dummy_logger
            <| create_dummy_transport event.Publish 
            <| (fun agent -> async {
                let lock = LockData.create "123114141" |> Result.get
                let key = KeyData.create lock
                let pk = PkData.create "whatever" |> Result.get

                do! Async.Sleep timeout
                
                test <@ !create_dummy_transport_has_been_called @>

                event.Trigger <| Lock {
                    lock = lock
                    pk = pk
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = WaitingForAuth { 
                    connect_info = connect_info
                    key = key
                    nick = nick_data } @>
                test <@ !write_has_been_called @>

                event.Trigger <| Hello {
                    nick = nick_data
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = LoggedIn { 
                    connect_info = connect_info
                    nick = nick_data
                    users = UserMap.empty } @>
            })
            <| connect_info
            <| (nick_data, None)
        
        test <@ Result.isSuccess res @>

    [<Fact>]
    let ``Should start queue and pass authentication if first nick is denied`` () =
        let create_dummy_transport_has_been_called = ref false
        let write_has_been_called = ref 0
        let create_dummy_transport ievent connect_info =
            create_dummy_transport_has_been_called := true
            { new ITransport with
                member __.Received = ievent
                member __.Write (x) = write_has_been_called := !write_has_been_called + 1
                member __.Dispose() = () } 
            |> Success

        let event = new Event<DcppReceiveMessage>() 
    
        let connect_info = {
            host = HostnameData.create "localhost" |> Result.get
            port = PortData.create 411 |> Result.get
        }
        let nick_data = NickData.create "MnZrKk" |> Result.get
        let nick_data' = NickData.create "MnZrKk1" |> Result.get

        let res = 
            start_queue
            <| create_dummy_logger
            <| create_dummy_transport event.Publish
            <| (fun agent -> async {
                let lock = LockData.create "123114141" |> Result.get
                let key = KeyData.create lock
                let pk = PkData.create "whatever" |> Result.get
                
                do! Async.Sleep timeout
                test <@ !create_dummy_transport_has_been_called @>

                event.Trigger <| Lock {
                    lock = lock
                    pk = pk
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = WaitingForAuth {
                    connect_info = connect_info
                    key = key
                    nick = nick_data } @>
                test <@ !write_has_been_called = 1 @>
                event.Trigger <| ValidateDenied
                do! Async.Sleep timeout

                test <@ !write_has_been_called = 2 @>
                event.Trigger <| Hello {
                    nick = nick_data'
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = LoggedIn { 
                    connect_info = connect_info
                    nick = nick_data'
                    users = UserMap.empty } @>
            })
            <| connect_info
            <| (nick_data, None)
        
        test <@ Result.isSuccess res @>

    [<Fact>]
    let ``Should start queue and pass authentication with password`` () =
        let create_dummy_transport_has_been_called = ref false
        let write_has_been_called = ref 0
        let create_dummy_transport ievent connect_info =
            create_dummy_transport_has_been_called := true
            { new ITransport with
                member __.Received = ievent
                member __.Write (x) = write_has_been_called := !write_has_been_called + 1
                member __.Dispose() = () } 
            |> Success

        let event = new Event<DcppReceiveMessage>() 
    
        let connect_info = {
            host = HostnameData.create "localhost" |> Result.get
            port = PortData.create 411 |> Result.get
        }
        let nick_data = NickData.create "MnZrKk" |> Result.get
        let pass_data = PasswordData.create "dummypassword" |> Result.get

        let res = 
            start_queue
            <| create_dummy_logger
            <| create_dummy_transport event.Publish
            <| (fun agent -> async {
                let lock = LockData.create "123114141" |> Result.get
                let key = KeyData.create lock
                let pk = PkData.create "whatever" |> Result.get
                
                do! Async.Sleep timeout
                test <@ !create_dummy_transport_has_been_called @>

                event.Trigger <| Lock {
                    lock = lock
                    pk = pk
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = WaitingForAuth { 
                    connect_info = connect_info
                    key = key
                    nick = nick_data } @>
                test <@ !write_has_been_called = 1 @>

                event.Trigger <| GetPass
                do! Async.Sleep timeout

                test <@ !write_has_been_called = 2 @>
                event.Trigger <| Hello {
                    nick = nick_data
                }
                do! Async.Sleep timeout

                test <@ agent.fetch() |> Result.get |> fst = LoggedIn { 
                    connect_info = connect_info
                    nick = nick_data
                    users = UserMap.empty } @>
            })
            <| connect_info
            <| (nick_data, Some pass_data)
        
        test <@ Result.isSuccess res @>

module ``infrastructure Tests`` =
    [<Fact>]
    let ``Should convert example Lock message`` () =
        let input = "$Lock EXTENDEDPROTOCOLL\98W0q0:gmyMHSWL1qN4>v9YkYwg6 Pk=PtokaX|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (Lock {
            lock = LockData.create "EXTENDEDPROTOCOLL\98W0q0:gmyMHSWL1qN4>v9YkYwg6" |> Result.get
            pk = PkData.create "PtokaX" |> Result.get
        }) @> 

    [<Fact>]
    let ``Should convert example NickList message`` () =
        let input = "$NickList PtokaX$$TestUser$$MnZrK$$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (NickList {
            nicks = [ "PtokaX"; "TestUser"; "MnZrK"] |> List.map (NickData.create >> Result.get)
        }) @> 

    [<Fact>]
    let ``Should convert example ChatMessage message`` () =
        let input = "<Baloo> Hello! (= |"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (ChatMessage {
            nick = NickData.create "Baloo" |> Result.get
            message = "Hello! (= " 
        }) @> 
        
    [<Fact>]
    let ``Should convert ChatMessage message with newlines and tabs`` () =
        let input = "<Baloo> Hello! (= \n qwer \t asdf|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (ChatMessage {
            nick = NickData.create "Baloo" |> Result.get
            message = "Hello! (= \n qwer \t asdf" 
        }) @> 
        
    [<Fact>]
    let ``Should convert another ChatMessage example`` () =
        let input = "<Baloo>\nHello! (= \n qwer \t asdf|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (ChatMessage {
            nick = NickData.create "Baloo" |> Result.get
            message = "Hello! (= \n qwer \t asdf" 
        }) @> 
                
    [<Fact>]
    let ``Should convert MyInfo example`` () =
        let input = "$MyINFO $ALL PtokaX $ $$$$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (DcppReceiveMessage.MyInfo {
            nick = NickData.create "PtokaX" |> Result.get
            share_size = PositiveInt.fromULong 0UL 
        }) @> 

    [<Fact>]
    let ``Should convert another MyInfo example`` () =
        let input = "$MyINFO $ALL MnZrKk $ $50A$$0$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (DcppReceiveMessage.MyInfo {
            nick = NickData.create "MnZrKk" |> Result.get
            share_size = PositiveInt.fromULong 0UL
        }) @>

    [<Fact>]
    let ``Should convert yet another MyInfo example`` () =
        let input = "$MyINFO $ALL MnZrKk $ $50A$$1000$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (DcppReceiveMessage.MyInfo {
            nick = NickData.create "MnZrKk" |> Result.get
            share_size = PositiveInt.fromULong 1000UL 
        }) @>

    [<Fact>]
    let ``Should convert Flylink MyInfo example`` () =
        let input = "$MyINFO $ALL TestUser [15]$A$50A$$2948599916$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (DcppReceiveMessage.MyInfo {
            nick = NickData.create "TestUser" |> Result.get
            share_size = PositiveInt.fromULong 2948599916UL 
        }) @>

    [<Fact>]
    let ``Should convert another real-life MyInfo example`` () =
        let input = "$MyINFO $ALL oksanasumy [13]<FlylinkDC++ V:r415,M:P,H:72/0/4,S:15>$ $50$$108650946339$|"

        let res = DCNstring_to_DcppMessage input
        test <@ res = Success (DcppReceiveMessage.MyInfo {
            nick = NickData.create "oksanasumy" |> Result.get
            share_size = PositiveInt.fromULong 108650946339UL 
        }) @>
        