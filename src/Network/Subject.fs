namespace Solver.Network

open System
open System.Collections.Generic


type Solution =
    | Optimal of Dictionary<int * int, int> * Dictionary<int * int, int>
    | Feasible
    | Infeasible
    | Unbounded

module Subject =

    type T = Subject of n: int[] * cost: IDictionary<int * int, int> * capacity: IDictionary<int * int, int>

    let init (n: int[]) cost capacity =
        if n[0] <> 0 then
            invalidArg (nameof n) $"Supply of node 0 must be zero."

        if Array.sum n <> 0 then
            invalidArg (nameof n) $"Summary of supply must be zero."

        let mutable c = Int32.MaxValue

        match
            n[1..]
            |> Array.forall (fun e ->
                let result = e <= c
                c <- e
                result)
        with
        | true -> Subject(n, cost, capacity)
        | _ -> invalidArg (nameof n) $"Supply of nodes must sorted descendent."

    let value (Subject(n, cost, capacity)) = n, cost, capacity
