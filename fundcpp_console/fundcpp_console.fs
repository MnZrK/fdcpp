module fundcpp_console


/// Creates a new string whose characters are the result of applying 
/// the function mapping to each of the characters of the input string
/// and concatenating the resulting strings.
let myfunc = fun x -> 10 + x

[<EntryPoint>]
let main argv =
    printfn "This is an integer %d" (myfunc 5)
    printfn "%A" argv
    0 // return an integer exit code
