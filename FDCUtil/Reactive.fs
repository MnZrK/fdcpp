[<AutoOpen>]
module FDCUtil.Reactive

open System
open System.Threading

open FSharp.Control.Reactive
open FSharpx.Control

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a, 'b) asyncresult = Async<result<'a,'b>>
type AsyncResult<'a, 'b> = asyncresult<'a, 'b>
module AsyncResult = 
    let ofAsyncChoice2 c = Async.map Result.ofChoice2 c
    let ofResult r = async { return r }

    let map f m = async {
        let! res = m
        return Result.map f res
    } 
    let (<?>) = map

    let bindSuccess m f = async {
        let! res = m
        return! 
            match res with
            | Failure x ->  async { return Failure x }
            | Success x -> f x
    }
    let bind = bindSuccess
    let (>>=) = bind

    type SuccessBuilder() =
        member __.Bind(m, f) = bindSuccess m f
        member __.Return(x) = async { return Success x }
        member __.ReturnFrom(x) = x
    let success_workflow = new SuccessBuilder()    

module Array =
    /// Fetch the data using `readf_async`, "concatenate" all the data in a single stream
    /// and then split it into messages where end of each message is indicated by `eom_marker`
    let fetchConcatSplit som_marker_maybe eom_marker readf_async =
        let subject = new Subject<'a[]>()

        let rec loop_find_eom loop acc msg = async {
            let i_maybe = Array.tryFindIndex ((=) eom_marker) msg
            match i_maybe with
            | None ->
                return! loop (Some (msg::acc))
            | Some i ->
                if i < (msg.Length - 1) then
                    let msg' = 
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    subject.OnNext msg'
                    match som_marker_maybe with
                    | None -> return! loop_find_eom loop [] msg.[(i+1)..(msg.Length-1)]
                    | Some som_marker ->
                        // TODO what if binary file starts with som_marker? need to fix it
                        if msg.[i+1] = som_marker then
                            return! loop_find_eom loop [] msg.[(i+1)..(msg.Length-1)]
                        else    
                            subject.OnNext(msg.[(i+1)..(msg.Length-1)])
                            return! loop None
                else
                    let msg' =
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    subject.OnNext msg'
                    return! loop (Some [])
        }

        let rec loop acc_maybe = async {
            let! msg_res = readf_async()
            match msg_res with
            | Failure ex ->
                subject.OnError(ex)
            | Success [||] ->
                subject.OnCompleted() // TODO check again all the error handling and disposing! of subjects and whatnot. remember that there are two different subjects with the same name, and one of them is disposable and another is not
            | Success msg ->
                match acc_maybe with
                | None -> 
                    subject.OnNext(msg)
                    return! loop None
                | Some [] ->
                    match som_marker_maybe with
                    | None -> return! loop_find_eom loop [] msg 
                    | Some som_marker ->
                        if msg.[0] = som_marker then
                            return! loop_find_eom loop [] msg
                        else    
                            subject.OnNext(msg)
                            return! loop None
                | Some acc ->
                    return! loop_find_eom loop acc msg
        }
        let cts = new CancellationTokenSource()
        let lazystart = lazy (Async.Start(loop (Some []), cts.Token))

        let obs = Observable.asObservable subject
        let deferred = 
            Observable.defer (fun () ->
                lazystart.Force()
                obs
            )
        let dispose =
            (fun () -> 
                cts.Cancel()
                (cts :> IDisposable).Dispose()
                subject.OnCompleted()
            )
            |> callable_once

        deferred, dispose
