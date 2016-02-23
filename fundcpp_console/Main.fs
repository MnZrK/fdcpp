module fundcpp_console

open System.Threading

[<EntryPoint>]
let main argv =
    printfn "Hello world from fundcpp console! args: %A" argv
    printfn "result from library: %d" (fundcpp_net.Main.my_main 5)
    printf "Starting TCP server..."
    
    let disposable = fundcpp_net.Tcp.Server.Start(port = 8090)
    Thread.Sleep(60 * 1000)
    printfn "bye!"
    disposable.Dispose()
        
    0 // return an integer exit code
