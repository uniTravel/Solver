namespace Solver.Network

open System.Collections.Generic


type Solution =
    | Optimal of Dictionary<int * int, int> * Dictionary<int * int, int>
    | Feasible
    | Infeasible
    | Unbounded

module Subject =

    type T = Subject of n: int[] * cost: IDictionary<int * int, int> * capacity: IDictionary<int * int, int>

    let bfs len (adjacent: int list list) =
        let n = Array.zeroCreate<int> len
        let queue = Queue<int>(len)
        n[0] <- 1
        queue.Enqueue 1
        n[1] <- 1

        while queue.Count <> 0 do
            adjacent[queue.Dequeue()]
            |> List.iter (fun i ->
                match n[i] with
                | 0 ->
                    queue.Enqueue i
                    n[i] <- 1
                | _ -> ())

        n |> Array.exists (fun e -> e = 0)

    let init (n: int[]) (adjacent: int list list) (cost: IDictionary<int * int, int>) capacity =
        if n[0] <> 0 then
            invalidArg (nameof n) $"Supply of node 0 must be zero."

        if Array.sum n <> 0 then
            invalidArg (nameof n) $"Summary of supply must be zero."

        if cost |> Seq.exists (fun (KeyValue((t, h), _)) -> t = 0 || h = 0) then
            invalidArg (nameof cost) $"Root must have no connected arc."

        if n.Length - cost.Count > 2 then
            invalidArg (nameof cost) $"Graph must be connected."

        let pro = HashSet<int * int> cost.Count
        let anti = HashSet<int * int> cost.Count

        ((pro, anti), List.indexed adjacent)
        ||> List.fold (fun (pro, anti) (i, jl) ->
            jl
            |> List.iter (fun j ->
                match (i, j) with
                | i, j when cost.ContainsKey(i, j) -> pro.Add(i, j) |> ignore
                | i, j when cost.ContainsKey(j, i) -> anti.Add(i, j) |> ignore
                | _ -> invalidArg (nameof adjacent) $"Graph doesn't contain arc ({i},{j}).")

            pro, anti)
        |> ignore

        if pro.Count <> cost.Count || anti.Count <> cost.Count then
            invalidArg (nameof adjacent) $"Adjacent unmatched with arcs."

        if bfs n.Length adjacent then
            invalidArg (nameof cost) $"Graph must be connected."

        Subject(n, cost, capacity)

    let value (Subject(n, cost, capacity)) = n, cost, capacity
