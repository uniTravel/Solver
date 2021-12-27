namespace Benchmark.Simplex

open BenchmarkDotNet.Running


module Main =

    [<EntryPoint>]
    let main argv =
        let switch = BenchmarkSwitcher [| typeof<Multiply> |]
        switch.Run argv |> ignore
        0