module FDCUtil.Tests.Net

open System

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

open FDCTestHelper

open FDCUtil.Net

let ``Should parse stream`` buffer_size = 
    let input = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|$HubName CN.Peers|$Hello MnZrKk|"

    let stream = generate_stream_from_string input
    let eom_marker = Convert.ToByte '|'

    let sequence = concat_and_split_stream buffer_size eom_marker stream
    
    let arr = sequence |> Seq.toArray |> Array.map System.Text.Encoding.ASCII.GetString
    
    test <@ arr.[0] = "$Lock EXTENDEDPROTOCOL_hub Pk=1.0.9.MegaHub specific|" @>
    test <@ arr.[1] = "<Bender-Nsk-2> ????? ??????: 45 ???? 10 ????? 48 ????? 18 ??????. ????????????? ??????: 27159|" @>
    test <@ arr.[2] = "$HubName CN.Peers|" @>
    test <@ arr.[3] = "$Hello MnZrKk|" @>  

[<Fact>]
let ``Should parse stream with large buffer`` () =
    ``Should parse stream`` 512 

[<Fact>]
let ``Should parse stream from several pieces`` () =
     ``Should parse stream`` 16
