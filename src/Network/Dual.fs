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
            cap: IDictionary<int * int, int> *
            p: int[] *
            x: Dictionary<int * int, int>

    let augment sink (Dual(max, g, adj, inv, cost, cap, p, x)) (ind: int[]) (supply: HashSet<int>) =
        let rec trace j path =
            match ind[j] with
            | i when i = j -> path, i
            | i when i < 0 ->
                let i = -i
                trace i <| ((j, i), x[(j, i)], x[(j, i)], (-)) :: path
            | i when cap.ContainsKey(i, j) -> trace i <| ((i, j), cap[(i, j)] - x[(i, j)], x[(i, j)], (+)) :: path
            | i -> trace i <| ((i, j), max, x[(i, j)], (+)) :: path

        let ap, i = trace sink []
        let _, d, _, _ = ap |> List.minBy (fun (_, d, _, _) -> d)
        let d = [ g[i]; -g[sink]; d ] |> List.min
        ap |> List.iter (fun (k, _, f, op) -> x[k] <- op f d)
        g[i] <- g[i] - d
        g[sink] <- g[sink] + d
        if g[i] = 0 then supply.Remove i |> ignore else ()

    let create sub =
        let n, adj, inv, cost, cap = Subject.value sub
        let max = (n |> Array.filter (fun e -> e > 0) |> Array.sum) + 1
        let g = Array.copy n
        let p = Array.zeroCreate<int> n.Length
        let x = Dictionary<int * int, int> n.Length
        cost |> Seq.iter (fun (KeyValue(k, _)) -> x[k] <- 0)
        Dual(max, g, adj, inv, cost, cap, p, x)

    let pd (Dual(max, g, adj, inv, cost, cap, p, x) as sub) =
        let mutable sink = 0
        let todo = Queue<int> g.Length
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0) |> HashSet

        let upper i j =
            cap.ContainsKey(i, j) && x[(i, j)] = cap[(i, j)]

        let check i j =
            ind[j] <- i
            if g[j] = 0 then todo.Enqueue j else sink <- j

        let price (ind: int[]) =
            let min (d, mini as dm) i j =
                function
                | true ->
                    match p[j] + cost[(i, j)] - p[i] with
                    | r when r < 0 -> dm
                    | r when r > d -> dm
                    | r when r = d -> d, (i, j, true) :: mini
                    | r -> r, [ i, j, true ]
                | false ->
                    match p[j] - cost[(j, i)] - p[i] with
                    | r when r < 0 -> dm
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
                                | 0 -> if upper i j then dm else min dm i j true
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

        let rec iter (todo: Queue<int>) (ind: int[]) (supply: HashSet<int>) =
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

                        iter todo ind supply
                | _ ->
                    let i = todo.Dequeue()

                    adj[i]
                    |> Array.iter (fun j ->
                        match ind[j] with
                        | 0 ->
                            match upper i j with
                            | true -> ()
                            | false -> if p[j] + cost[(i, j)] - p[i] = 0 then check i j else ()
                        | _ -> ())

                    inv[i]
                    |> Array.iter (fun j ->
                        match ind[j] with
                        | 0 ->
                            match x[(j, i)] with
                            | 0 -> ()
                            | _ -> if p[j] - cost[(j, i)] - p[i] = 0 then check -i j else ()
                        | _ -> ())

                    iter todo ind supply
            | _, s ->
                augment s sub ind supply
                todo.Clear()
                g |> Array.iteri (fun i e -> if e > 0 then ind[i] <- i else ind[i] <- 0)
                supply |> Seq.iter (fun e -> todo.Enqueue e)
                sink <- 0
                iter todo ind supply

        supply |> Seq.iter (fun e -> todo.Enqueue e)
        iter todo ind supply

    let ssp (Dual(max, g, adj, inv, cost, cap, p, x) as sub) : Solution =
        let rec iter len (dist: int[]) (todo: Dictionary<int, Queue<int>>) (ind: int[]) =
            function
            | (supply: HashSet<int>) when supply.Count = 0 -> Optimal x
            | supply ->
                let rec run sink k =
                    let check i j r =
                        match dist[i] + r with
                        | d when d < dist[j] ->
                            dist[j] <- d
                            if todo.ContainsKey d then () else todo[d] <- Queue<int>()
                            todo[d].Enqueue j
                        | _ -> ()

                    let scan k =
                        match todo[k].Dequeue() with
                        | i when g[i] < 0 -> run i k
                        | i ->
                            adj[i]
                            |> Array.iter (fun j ->
                                match x[(i, j)] with
                                | x when cap.ContainsKey(i, j) && x = cap[(i, j)] -> ()
                                | _ ->
                                    match p[j] + cost[(i, j)] - p[i] with
                                    | r when r >= 0 ->
                                        ind[j] <- i
                                        check i j r
                                    | _ -> ())

                            inv[i]
                            |> Array.iter (fun j ->
                                match x[(j, i)] with
                                | 0 -> ()
                                | _ ->
                                    match p[j] - cost[(j, i)] - p[i] with
                                    | r when r >= 0 ->
                                        ind[j] <- -i
                                        check i j r
                                    | _ -> ())

                            run 0 k

                    match sink, todo.Count with
                    | 0, 1 ->
                        match todo[k].Count with
                        | 0 -> Infeasible
                        | _ -> scan k
                    | 0, _ ->
                        match todo[k].Count with
                        | 0 ->
                            todo.Remove k |> ignore
                            todo.Keys |> Seq.min |> scan
                        | _ -> scan k
                    | sink, _ ->
                        dist |> Array.iteri (fun i d -> if k > d then p[i] <- p[i] + k - d else ())
                        augment sink sub ind supply
                        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
                        iter k dist todo ind supply

                todo.Clear()
                todo[0] <- Queue supply
                run 0 0

        let dist = Array.create g.Length Int32.MaxValue
        let todo = Dictionary<int, Queue<int>>()
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0)
        dist[0] <- 0
        supply |> Array.iter (fun e -> dist[e] <- 0)
        HashSet supply |> iter Int32.MaxValue dist todo ind

    let rex (sub: T) : Solution =

        failwith ""
