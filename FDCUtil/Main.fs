[<AutoOpen>]
module FDCUtil.Main

open System

let ct a _ = a

let callable_once fn =
    let lazyf = lazy (fn())
    fun () -> lazyf.Value

let tee fn x = x |> fn |> ignore; x
let ( |>! ) x fn = tee fn x

/// Do not use directly
type MapWithArbKeyType<'key, 'value> when 'key : comparison = MapWithArbKeyType of Map<'key, 'value>
[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>] 
type MapWithArbKey<'key, 'value> when 'key : comparison (construct_key: 'value -> 'key) = 
    member __.add value (MapWithArbKeyType map) = 
        let key = construct_key value
        MapWithArbKeyType (Map.add key value map) 

    member __.addIfMissing value (MapWithArbKeyType map) =
        let key = construct_key value
        if Map.containsKey key map then map
        else Map.add key value map
        |> MapWithArbKeyType

    member __.find key (MapWithArbKeyType map) = 
        Map.find key map

    member __.tryFind key (MapWithArbKeyType map) =
        Map.tryFind key map

    member __.containsKey key (MapWithArbKeyType map) = 
        Map.containsKey key map

    member __.empty = 
        MapWithArbKeyType Map.empty<'key, 'value>

    member __.remove key (MapWithArbKeyType map) = 
        Map.remove key map |> MapWithArbKeyType

[<AutoOpen>]
module BaseTypeExtensions = 
    module Array =
        let concatSplit eom_marker chunks = 
            let rec loop_find_eom loop acc msg = seq {
                let i_maybe = Array.tryFindIndex ((=) eom_marker) msg
                match i_maybe with
                | None ->
                    yield! loop (msg::acc)
                | Some i ->
                    if i < (msg.Length - 1) then
                        yield
                            msg.[0..i]::acc
                            |> List.rev
                            |> Array.concat
                        yield! loop_find_eom loop [] msg.[(i+1)..(msg.Length-1)]
                    else
                        yield
                            msg.[0..i]::acc
                            |> List.rev
                            |> Array.concat
                        yield! loop []
            }
            let rec loop acc = seq {
                for chunk in chunks do yield! loop_find_eom loop acc chunk
            }
            loop []

    module String =
        let split (sep:string) (str:string) =
            match sep, str with
            | ((null | ""), _) | (_, (null | "")) -> seq [str]
            | _ ->
            let n, j = str.Length, sep.Length
            let rec loop p = 
                seq {
                if p < n then
                    let i = match str.IndexOf(sep, p) with -1 -> n | i -> i
                    yield str.Substring(p, i - p)
                    yield! loop (i + j)
                }
            loop 0

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a,'b) result =
| Success of 'a
| Failure of 'b
type Result<'a, 'b> = result<'a, 'b> 
module Result =
    let ofOption e o =
        match o with 
        | None -> Failure e
        | Some x -> Success x
    let toOption r =
        match r with
        | Failure _ -> None
        | Success x -> Some x

    let ofChoice2 =
        function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    // type Aggregated<'a, 'b> =
    // | Inner of 'a
    // | Outer of 'b

    // let flattenSuccess (mm: Result<Result<'a,'c>,'b>) =
    //     match mm with
    //     | Failure e -> Outer e |> Failure
    //     | Success m ->
    //         match m with
    //         | Failure e -> Inner e |> Failure
    //         | Success x -> Success x

    let isSuccess = 
        function
        | Success _ -> true
        | Failure _ -> false

    let mapSuccess f =
        function
        | Success x -> f x |> Success
        | Failure x -> Failure x
    let map = mapSuccess
    let (<?>) = map

    let mapFailure f =
        function
        | Success x -> Success x
        | Failure x -> f x |> Failure

    let bindSuccess m f = 
        match m with
        | Success x -> f x
        | Failure x -> Failure x
    let bind = bindSuccess
    let (>>=) = bind

    let bindFailure m f = 
        match m with
        | Success x -> Success x
        | Failure x -> f x

    let applySuccess mF m =
        match mF with
        | Success f -> mapSuccess f m
        | Failure x -> Failure x
    let apply = applySuccess
    let (<*>) = apply

    let applyFailure mF m = 
        match mF with
        | Success x -> Success x 
        | Failure f -> mapFailure f m

    let fork successF failureF m = 
        match m with
        | Success x -> successF x
        | Failure x -> failureF x

    let getSuccess m = 
        match m with
        | Success x -> 
            x
        | Failure x -> 
            raise (new System.InvalidOperationException(sprintf "Trying to get Success from Failure: %A" x))
    let get = getSuccess

    let getFailure m = 
        match m with
        | Success x -> 
            raise (new System.InvalidOperationException(sprintf "Trying to get Failure from Success: %A" x))
        | Failure x -> 
            x

    let collect f m = bind m f 

    type SuccessBuilder() =
        member __.Bind(m, f) = bindSuccess m f
        member __.Return(x) = Success x
        member __.ReturnFrom(x) = x
    type SuccessWithStringFailureBuilder() =
        let stringify (x: obj) =
            match x with
            | :? string as str -> str
            | x -> sprintf "%A" x

        member __.Bind(m, f) =
            (m |> mapFailure stringify) 
        >>= (f >> mapFailure stringify)
        
        member __.Return(x) = Success x
        member __.ReturnFrom(x) = x |> mapFailure stringify 
    let success_workflow = new SuccessBuilder()
    let success_workflow_with_string_failures = new SuccessWithStringFailureBuilder()

    type FailureBuilder() =
        member __.Bind(m, f) = bindFailure m f
        member __.Return(x) = Failure x
        member __.ReturnFrom(x) = x
    let failure_workflow = new FailureBuilder()

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a, 'b) resultlist = result<'a, 'b> list
type ResultList<'a, 'b> = resultlist<'a, 'b> 
module ResultList =
    let swap (xs: resultlist<'a, _>): result<'a list, _> =
        let error = 
            xs 
            |> List.tryPick (
                function
                | Success x -> None
                | Failure x -> Failure x |> Some
            ) 
        
        match error with
        | Some x -> x
        | None -> 
            xs
            |> List.choose (
                function
                | Failure x -> None
                | Success x -> x |> Some
            )
            |> Success

    let bind (m: result<'x list, _>) (f: 'x -> result<'x list, _>): result<'x list, _> =
        match m with
        | Success xs ->
            xs
            |> List.map f 
            |> swap 
            |> Result.map List.concat
        | Failure x -> 
            Failure x 
    let (>>=) = bind
