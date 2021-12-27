namespace Benchmark.Simplex

open System
open System.Numerics
open BenchmarkDotNet.Attributes


[<MemoryDiagnoser>]
[<LongRunJob>]
type Multiply() =

    let rand = Random 100
    let mutable acc1 = 0.0
    let mutable acc2 = 0.0
    let chunkSize = Vector<float>.Count


    [<Params(100, 10000, 1000000)>]
    member val count = 0 with get, set

    [<DefaultValue>]
    val mutable arr1: float []

    [<DefaultValue>]
    val mutable arr2: float []

    [<IterationSetup>]
    member me.IterationSetup() =
        acc1 <- 0.0
        acc2 <- 0.0
        me.arr1 <- Array.create me.count (rand.NextDouble())
        me.arr2 <- Array.create me.count (rand.NextDouble())

    [<Benchmark(Baseline = true)>]
    member me.Array() =
        for i in 0 .. me.count - 1 do
            acc1 <- me.arr1.[i] * me.arr2.[i] + acc1
        acc1

    [<Benchmark>]
    member me.Fold() =
        Array.fold2 (fun acc l r -> acc + l * r) 0.0 me.arr1 me.arr2

    [<Benchmark>]
    member me.Vector() =
        let rec fast n =
            if n > me.arr1.Length - chunkSize then
                for i in n .. me.count - 1 do
                    acc2 <- me.arr1.[i] * me.arr2.[i] + acc2
                acc2
            else
                acc2 <- Vector.Dot(Vector(me.arr1, n), Vector(me.arr2, n)) + acc2
                fast <| n + chunkSize
        fast 0