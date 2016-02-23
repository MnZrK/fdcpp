module fundcpp_console

[<EntryPoint>]
let main argv =
    printfn "Hello world from fundcpp console! args: %A" argv
    printfn "result from library: %d" (fundcpp_net.Main.my_main 5)
    0 // return an integer exit code
