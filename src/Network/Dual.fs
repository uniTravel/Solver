namespace Solver.Network

open System
open System.Collections.Generic


module Dual =

    type T =
        | Dual of
            g: int[] *
            adj: int[][] *
            inv: int[][] *
            cost: IDictionary<int * int, int> *
            cap: IDictionary<int * int, int> *
            p: int[] *
            x: Dictionary<int * int, int>

    let augment sink (Dual(g, adj, inv, cost, cap, p, x)) (ind: int[]) (supply: HashSet<int>) =
        let rec trace j path =
            match ind[j] with
            | i when i = j -> path, i
            | i when i < 0 ->
                let i = -i
                trace i <| ((j, i), x[(j, i)], x[(j, i)], (-)) :: path
            | i -> trace i <| ((i, j), cap[(i, j)] - x[(i, j)], x[(i, j)], (+)) :: path

        let ap, i = trace sink []
        let _, d, _, _ = ap |> List.minBy (fun (_, d, _, _) -> d)
        let d = [ g[i]; -g[sink]; d ] |> List.min
        ap |> List.iter (fun (k, _, f, op) -> x[k] <- op f d)
        g[i] <- g[i] - d
        g[sink] <- g[sink] + d
        if g[i] = 0 then supply.Remove i |> ignore else ()

    let create sub =
        let n, adj, inv, cost, cap = Subject.value sub

        if
            cap.Count <> cost.Count
            || (Seq.sort cap.Keys, Seq.sort cost.Keys) ||> Seq.exists2 (fun e1 e2 -> e1 <> e2)
        then
            invalidArg (nameof sub) $"Arcs must have upper bound."

        let g = Array.copy n
        let p = Array.zeroCreate<int> n.Length
        let x = Dictionary<int * int, int> n.Length

        cost
        |> Seq.iter (function
            | KeyValue((i, j as k), v) when v < 0 ->
                x[k] <- cap[k]
                g[i] <- g[i] - x[k]
                g[j] <- g[j] + x[k]
            | KeyValue(k, _) -> x[k] <- 0)

        Dual(g, adj, inv, cost, cap, p, x)

    let pd (Dual(g, adj, inv, cost, cap, p, x) as sub) =
        let mutable sink = 0
        let todo = Queue<int> g.Length
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0) |> HashSet
        supply |> Seq.iter (fun e -> todo.Enqueue e)

        let check i j =
            ind[j] <- i
            if g[j] = 0 then todo.Enqueue j else sink <- j

        let price (ind: int[]) =
            let min (d, mini as dm) i j =
                function
                | true ->
                    match p[j] + cost[(i, j)] - p[i] with
                    | r when r > d -> dm
                    | r when r = d -> d, (i, j, true) :: mini
                    | r -> r, [ i, j, true ]
                | false ->
                    match p[j] - cost[(j, i)] - p[i] with
                    | r when r > d -> dm
                    | r when r = d -> d, (i, j, false) :: mini
                    | r -> r, [ i, j, false ]

            match
                ((Int32.MaxValue, []), Array.indexed ind)
                ||> Array.fold (fun dm (i, e) ->
                    match e with
                    | 0 -> dm
                    | _ ->
                        Array.fold
                            (fun dm j ->
                                match ind[j] with
                                | 0 -> if x[(i, j)] = cap[(i, j)] then dm else min dm i j true
                                | _ -> dm)
                            dm
                            adj[i]
                        |> Array.foldBack
                            (fun j dm ->
                                match ind[j] with
                                | 0 -> if x[(j, i)] = 0 then dm else min dm i j false
                                | _ -> dm)
                            inv[i])
            with
            | Int32.MaxValue, _ -> []
            | d, mini ->
                ind |> Array.iteri (fun i e -> if e <> 0 then p[i] <- p[i] + d else ())
                mini

        let rec iter () =
            match supply.Count, sink with
            | 0, _ -> Optimal x
            | _, 0 ->
                match todo.Count with
                | 0 ->
                    match price ind with
                    | [] -> Infeasible
                    | mini ->
                        mini
                        |> List.iter (fun (i, j, o) ->
                            match o, ind[j] with
                            | true, 0 -> check i j
                            | false, 0 -> check -i j
                            | _ -> ())

                        iter ()
                | _ ->
                    let i = todo.Dequeue()

                    adj[i]
                    |> Array.iter (fun j ->
                        match ind[j] with
                        | 0 ->
                            match x[(i, j)] with
                            | x when x = cap[(i, j)] -> ()
                            | _ -> if p[j] + cost[(i, j)] - p[i] = 0 then check i j else ()
                        | _ -> ())

                    inv[i]
                    |> Array.iter (fun j ->
                        match ind[j] with
                        | 0 ->
                            match x[(j, i)] with
                            | 0 -> ()
                            | _ -> if p[j] - cost[(j, i)] - p[i] = 0 then check -i j else ()
                        | _ -> ())

                    iter ()
            | _, s ->
                augment s sub ind supply
                todo.Clear()
                g |> Array.iteri (fun i e -> if e > 0 then ind[i] <- i else ind[i] <- 0)
                supply |> Seq.iter (fun e -> todo.Enqueue e)
                sink <- 0
                iter ()

        iter ()

    let ssp (Dual(g, adj, inv, cost, cap, p, x) as sub) : Solution =
        let dist = Array.create g.Length Int32.MaxValue
        let todo = Dictionary<int, Queue<int>>()
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0) |> HashSet

        let check i j o r =
            match dist[i] + r with
            | d when d < dist[j] ->
                dist[j] <- d
                ind[j] <- if o then i else -i
                if todo.ContainsKey d then () else todo[d] <- Queue<int>()
                todo[d].Enqueue j
            | _ -> ()

        let scan k =
            match todo[k].Dequeue() with
            | i when dist[i] <> k -> 0
            | i when g[i] < 0 -> i
            | i ->
                adj[i]
                |> Array.iter (fun j ->
                    match x[(i, j)] with
                    | x when x = cap[(i, j)] -> ()
                    | _ -> check i j true <| p[j] + cost[(i, j)] - p[i])

                inv[i]
                |> Array.iter (fun j ->
                    match x[(j, i)] with
                    | 0 -> ()
                    | _ -> check i j false <| p[j] - cost[(j, i)] - p[i])

                0

        let rec iter k sink =
            match supply.Count, sink with
            | 0, _ -> Optimal x
            | _, 0 ->
                match todo[k].Count with
                | 0 ->
                    match todo.Count with
                    | 1 -> Infeasible
                    | _ ->
                        todo.Remove k |> ignore
                        let k = Seq.min todo.Keys
                        iter k 0
                | _ -> iter k <| scan k
            | _, s ->
                augment s sub ind supply

                dist
                |> Array.iteri (fun i d ->
                    if k > d then p[i] <- p[i] + k - d else ()
                    if supply.Contains i then () else dist[i] <- Int32.MaxValue)

                todo.Clear()
                todo[0] <- Queue supply
                g |> Array.iteri (fun i e -> if e > 0 then ind[i] <- i else ind[i] <- 0)
                iter 0 0

        supply |> Seq.iter (fun e -> dist[e] <- 0)
        todo[0] <- Queue supply
        iter 0 0

    let rex (sub: T) : Solution =

        failwith ""
