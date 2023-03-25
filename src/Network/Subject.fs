namespace Solver.Network

open System.Collections.Generic


type Solution =
    | Optimal of Dictionary<int * int, int>
    | Feasible
    | Infeasible
    | Unbounded


module Subject =

    type T =
        | Subject of
            n: int[] *
            adj: int[][] *
            inv: int[][] *
            cost: IDictionary<int * int, int> *
            capacity: IDictionary<int * int, int>

    let bfs len (adjacent: int[][]) (inverse: int[][]) =
        let ind = Array.zeroCreate<int> len
        let queue = Queue<int> len
        let nodes = (adjacent, inverse) ||> Array.map2 (fun adj inv -> Array.append adj inv)
        ind[0] <- 1
        queue.Enqueue 1
        ind[1] <- 1

        while queue.Count <> 0 do
            nodes[queue.Dequeue()]
            |> Array.iter (fun i ->
                match ind[i] with
                | 0 ->
                    queue.Enqueue i
                    ind[i] <- 1
                | _ -> ())

        ind |> Array.exists (fun e -> e = 0)

    let init (n: int[]) (adj: int[][]) (inv: int[][]) (cost: IDictionary<int * int, int>) capacity =
        if n[0] <> 0 then
            invalidArg (nameof n) $"Supply of node 0 must be zero."

        if Array.sum n <> 0 then
            invalidArg (nameof n) $"Summary of supply must be zero."

        if cost |> Seq.exists (fun (KeyValue((t, h), _)) -> t = 0 || h = 0) then
            invalidArg (nameof cost) $"Root must have no connected arc."

        if n.Length - cost.Count > 2 then
            invalidArg (nameof cost) $"Graph must be connected."

        let pros = HashSet<int * int> cost.Count
        let cons = HashSet<int * int> cost.Count

        (adj, inv)
        ||> Array.iteri2 (fun i adj inv ->
            adj
            |> Array.iter (function
                | j when pros.Contains(i, j) -> invalidArg (nameof adj) $"Arc ({i},{j}) duplicated."
                | j when cost.ContainsKey(i, j) -> pros.Add(i, j) |> ignore
                | j -> invalidArg (nameof adj) $"Graph doesn't contain arc ({i},{j}).")

            inv
            |> Array.iter (function
                | j when cons.Contains(i, j) -> invalidArg (nameof inv) $"Arc ({i},{j}) duplicated."
                | j when cost.ContainsKey(j, i) -> cons.Add(i, j) |> ignore
                | j -> invalidArg (nameof inv) $"Graph doesn't contain arc ({j},{i})."))

        if pros.Count <> cost.Count then
            invalidArg (nameof adj) $"There has unmatched arcs."

        if cons.Count <> cost.Count then
            invalidArg (nameof inv) $"There has unmatched arcs."

        if bfs n.Length adj inv then
            invalidArg (nameof cost) $"Graph must be connected."

        Subject(n, adj, inv, cost, capacity)

    let value (Subject(n, adj, inv, cost, capacity)) = n, adj, inv, cost, capacity
