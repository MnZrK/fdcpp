namespace FDCTestHelper

module UnquoteExtensions =
    open Swensen.Unquote

    [<AutoOpen>]
    module Assertions =
        let testO (expr:Microsoft.FSharp.Quotations.Expr<bool>) =
            try test expr; None 
            with ex -> Some ex

    let inline try_raise res =
        match res with
        | Some ex -> raise ex
        | None -> ()
        
module FsCheckExtensions = 
    open FsCheck
    open FsCheck.Experimental

    let create_machine create_sut next = 
        let create initial_value =
            { new Setup<'suttype, 'modeltype>() with
                member __.Actual() = create_sut initial_value
                member __.Model() = initial_value }

        { new Machine<'suttype, 'modeltype>() with 
            member __.Setup = Arb.generate<'modeltype> |> Gen.map create |> Arb.fromGen 
            member __.Next(x) = next x }
    
    let create_operation label run check =
        { new Operation<'suttype, 'modeltype>() with
            override __.Run(m) = run m
            override __.Check(agent, m) = check agent m
            override __.ToString() = label }

    [<AutoOpen>]
    module Gen =
        let create1 f = Arb.generate<'a> |> Gen.map f