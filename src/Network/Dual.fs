namespace Solver.Network

open System
open System.Collections.Generic


module Dual =

    type T =
        | Dual of
            max: int *
            g: int[] *
            adj: int[][] *
            inv: int[][] *
            cost: IDictionary<int * int, int> *
            capacity: IDictionary<int * int, int> *
            p: int[] *
            x: Dictionary<int * int, int>

    let price (Dual(max, g, adj, inv, cost, capacity, p, x)) (ind: int[]) =
        let min (d, mini as de) i j =
            function
            | true ->
                match p[j] + cost[(i, j)] - p[i] with
                | r when r < 0 -> de
                | r when r > d -> de
                | r when r = d -> d, (i, j, true) :: mini
                | r -> r, [ i, j, true ]
            | false ->
                match p[j] - cost[(j, i)] - p[i] with
                | r when r < 0 -> de
                | r when r > d -> de
                | r when r = d -> d, (i, j, false) :: mini
                | r -> r, [ i, j, false ]

        match
            Array.indexed ind
            |> Array.filter (fun (_, e) -> e > 0)
            |> Array.map fst
            |> Array.fold
                (fun dm i ->
                    Array.fold (fun dm j -> min dm i j true) dm adj[i]
                    |> Array.foldBack (fun j dm -> min dm i j false) inv[i])
                (Int32.MaxValue, [])
        with
        | Int32.MaxValue, _ -> []
        | d, mini ->
            ind |> Array.iter (fun e -> if e > 0 then p[e] <- p[e] + d else ())
            mini

    let augment sink (Dual(max, g, adj, inv, cost, capacity, p, x)) (ind: int[]) (supply: HashSet<int>) (bv: int[]) =
        let rec trace j path =
            match ind[j] with
            | i when i = j -> path, i
            | i ->
                match bv[j] with
                | 0 ->
                    let d = if x.ContainsKey(j, i) then x[(j, i)] else 0
                    trace i <| ((j, i), d, d, (-)) :: path
                | -1 ->
                    let f = if x.ContainsKey(i, j) then x[(i, j)] else 0
                    trace i <| ((i, j), max, f, (+)) :: path
                | u ->
                    let d, f =
                        match x.ContainsKey(i, j) with
                        | true -> u - x[(i, j)], x[(i, j)]
                        | false -> u, 0

                    trace i <| ((i, j), d, f, (+)) :: path

        let ap, i = trace sink []
        let _, d, _, _ = ap |> List.minBy (fun (_, d, _, _) -> d)
        let d = [ g[i]; -g[sink]; d ] |> List.min
        ap |> List.iter (fun (k, _, f, op) -> x[k] <- op f d)
        g[i] <- g[i] - d
        g[sink] <- g[sink] + d
        if g[i] = 0 then supply.Remove i |> ignore else ()

    let create sub =
        let n, adj, inv, cost, capacity = Subject.value sub
        let max = (n |> Array.filter (fun e -> e > 0) |> Array.sum) + 1
        let g = Array.copy n
        let p = Array.zeroCreate<int> n.Length
        let x = Dictionary<int * int, int> n.Length
        Dual(max, g, adj, inv, cost, capacity, p, x)

    let pd (Dual(max, g, adj, inv, cost, capacity, p, x) as sub) =
        let rec iter (ind: int[]) =
            function
            | (supply: HashSet<int>) when supply.Count = 0 -> Optimal x
            | supply ->
                let rec run sink (bv: int[]) (scan: Queue<int>) =
                    match sink, scan.Count with
                    | 0, 0 ->
                        match price sub ind with
                        | [] -> Infeasible
                        | mini ->
                            mini
                            |> List.iter (fun (i, j, o) ->
                                if o then
                                    bv[j] <- if capacity.ContainsKey(i, j) then capacity[(i, j)] else -1

                                ind[j] <- i
                                if g[j] < 0 then run j bv scan |> ignore else scan.Enqueue j)

                            run 0 bv scan
                    | 0, _ ->
                        let i = scan.Dequeue()

                        adj[i]
                        |> Array.iter (fun j ->
                            match ind[j] with
                            | 0 ->
                                match p[j] + cost[(i, j)] - p[i] with
                                | 0 ->
                                    bv[j] <- if capacity.ContainsKey(i, j) then capacity[(i, j)] else -1
                                    ind[j] <- i
                                    if g[j] < 0 then run j bv scan |> ignore else scan.Enqueue j
                                | _ -> ()
                            | _ -> ())

                        inv[i]
                        |> Array.iter (fun j ->
                            match ind[j] with
                            | 0 ->
                                match p[j] - cost[(j, i)] - p[i] with
                                | 0 ->
                                    ind[j] <- i
                                    if g[j] < 0 then run j bv scan |> ignore else scan.Enqueue j
                                | _ -> ()
                            | _ -> ())

                        run 0 bv scan
                    | sink, _ ->
                        augment sink sub ind supply bv
                        let ind = g |> Array.mapi (fun i e -> if e > 0 then i else 0)
                        iter ind supply

                (Array.zeroCreate g.Length, Queue supply) ||> run 0

        let ind = g |> Array.mapi (fun i e -> if e > 0 then i else 0)
        ind |> Array.filter (fun e -> e <> 0) |> HashSet |> iter ind

    let ssp (sub: T) : Solution =

        failwith ""

    let rex (sub: T) : Solution =

        failwith ""
