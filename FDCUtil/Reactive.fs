[<AutoOpen>]
module FDCUtil.Reactive

open FSharp.Control.Reactive
open FSharpx.Control

[<System.Diagnostics.CodeAnalysis.SuppressMessage(Category = "NameConventions", CheckId = "*")>]
type ('a, 'b) asyncresult = Async<result<'a,'b>>
type AsyncResult<'a, 'b> = asyncresult<'a, 'b>
module AsyncResult = 
    let ofAsyncChoice2 c = Async.map Result.ofChoice2 c

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
    let fetchConcatSplit eom_marker readf_async ctoken =
        let subject = new Subject<'a[]>()

        let rec loop_find_eom loop acc msg = async {
            let i_maybe = Array.tryFindIndex ((=) eom_marker) msg
            match i_maybe with
            | None ->
                return! loop (msg::acc)
            | Some i ->
                if i < (msg.Length - 1) then
                    let msg' = 
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    subject.OnNext msg'
                    return! loop_find_eom loop [] msg.[(i+1)..(msg.Length-1)]
                else
                    let msg' =
                        msg.[0..i]::acc
                        |> List.rev
                        |> Array.concat
                    subject.OnNext msg'
                    return! loop []
        }

        let rec loop acc = async {
            let! msg_res = readf_async()
            match msg_res with
            | Failure ex ->
                subject.OnError(ex)
            | Success [||] ->
                subject.OnCompleted()
            | Success msg ->
                return! loop_find_eom loop acc msg
                // TODO send onCompleted when cancelled ?
        }
        let lazystart = lazy (Async.Start(loop [], ctoken))

        let obs = Observable.asObservable subject
        Observable.defer (fun () -> lazystart.Force(); obs)
