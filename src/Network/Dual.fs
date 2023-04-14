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
            r: Dictionary<int * int, int> *
            x: Dictionary<int * int, int> *
            aug: Stack<(int * int) * int * int * (int -> int -> int)>

    let augment sink (Dual(g, adj, inv, cost, cap, p, r, x, aug)) (ind: int[]) (supply: HashSet<int>) =
        let rec trace j d =
            match ind[j] with
            | i when i = j -> i, d
            | i when i < 0 ->
                let i = -i
                aug.Push((j, i), x[(j, i)], x[(j, i)], (-))
                trace i <| if x[(j, i)] < d then x[(j, i)] else d
            | i ->
                let v = cap[(i, j)] - x[(i, j)]
                aug.Push((i, j), v, x[(i, j)], (+))
                trace i <| if v < d then v else d

        let i, d = trace sink Int32.MaxValue
        let d = min g[i] d
        let d = min -g[sink] d

        while aug.Count <> 0 do
            let k, _, f, op = aug.Pop()
            x[k] <- op f d

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
        let r = Dictionary<int * int, int> cost
        let x = Dictionary<int * int, int> cost.Count
        let aug = Stack<(int * int) * int * int * (int -> int -> int)> n.Length

        cost
        |> Seq.iter (function
            | KeyValue((i, j as k), v) when v < 0 ->
                x[k] <- cap[k]
                g[i] <- g[i] - x[k]
                g[j] <- g[j] + x[k]
            | KeyValue(k, _) -> x[k] <- 0)

        Dual(g, adj, inv, cost, cap, p, r, x, aug)

    let pd (Dual(g, adj, inv, cost, cap, p, r, x, aug) as sub) =
        let mutable sink = 0
        let todo = Queue<int> g.Length
        let expand = Queue<int * int * bool> g.Length
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0) |> HashSet
        supply |> Seq.iter (fun e -> todo.Enqueue e)

        let check i j =
            match sink, ind[j] with
            | 0, 0 ->
                ind[j] <- i
                if g[j] = 0 then todo.Enqueue j else sink <- j
            | _ -> ()

        let price (ind: int[]) =
            let min d i j =
                function
                | true ->
                    match p[j] + cost[(i, j)] - p[i] with
                    | r when r > d -> d
                    | r when r = d ->
                        expand.Enqueue(i, j, true)
                        d
                    | r ->
                        expand.Clear()
                        expand.Enqueue(i, j, true)
                        r
                | false ->
                    match p[j] - cost[(j, i)] - p[i] with
                    | r when r > d -> d
                    | r when r = d ->
                        expand.Enqueue(i, j, false)
                        d
                    | r ->
                        expand.Clear()
                        expand.Enqueue(i, j, false)
                        r

            match
                (Int32.MaxValue, Array.indexed ind)
                ||> Array.fold (fun d (i, e) ->
                    match e with
                    | 0 -> d
                    | _ ->
                        Array.fold
                            (fun d j ->
                                match ind[j] with
                                | 0 -> if x[(i, j)] = cap[(i, j)] then d else min d i j true
                                | _ -> d)
                            d
                            adj[i]
                        |> Array.foldBack
                            (fun j dm ->
                                match ind[j] with
                                | 0 -> if x[(j, i)] = 0 then dm else min dm i j false
                                | _ -> dm)
                            inv[i])
            with
            | Int32.MaxValue -> false
            | d ->
                ind |> Array.iteri (fun i e -> if e <> 0 then p[i] <- p[i] + d else ())

                while expand.Count <> 0 do
                    match expand.Dequeue() with
                    | i, j, true ->
                        r[(i, j)] <- 0
                        check i j
                    | i, j, false ->
                        r[(j, i)] <- 0
                        check -i j

                true

        let rec iter () =
            match supply.Count, sink with
            | 0, _ -> Optimal x
            | _, 0 ->
                match todo.Count with
                | 0 ->
                    match price ind with
                    | false -> Infeasible
                    | true -> iter ()
                | _ ->
                    let i = todo.Dequeue()

                    adj[i]
                    |> Array.iter (fun j ->
                        match r[(i, j)] with
                        | 0 -> if x[(i, j)] = cap[(i, j)] then () else check i j
                        | _ -> ())

                    inv[i]
                    |> Array.iter (fun j ->
                        match r[(j, i)] with
                        | 0 -> if x[(j, i)] = 0 then () else check -i j
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

    let ssp (Dual(g, adj, inv, cost, cap, p, r, x, aug) as sub) : Solution =
        let dist = Array.create g.Length Int32.MaxValue
        let todo = Dictionary<int, Queue<int>>()
        let ind = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = ind |> Array.filter (fun e -> e <> 0) |> HashSet

        let check i j o d =
            match d with
            | d when d < dist[j] ->
                dist[j] <- d
                ind[j] <- if o then i else -i
                if todo.ContainsKey d then () else todo[d] <- Queue<int>()
                todo[d].Enqueue j
            | d when d = dist[j] && ind[j] = 0 ->
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
                    | _ ->
                        match r[(i, j)] with
                        | 0 -> check i j true dist[i]
                        | _ ->
                            r[(i, j)] <- p[j] + cost[(i, j)] - p[i]
                            check i j true <| dist[i] + r[(i, j)])

                inv[i]
                |> Array.iter (fun j ->
                    match x[(j, i)] with
                    | 0 -> ()
                    | _ ->
                        match r[(j, i)] with
                        | 0 -> check i j false dist[i]
                        | _ ->
                            r[(j, i)] <- p[j] - cost[(j, i)] - p[i]
                            check i j false <| dist[i] + r[(j, i)])

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
                dist |> Array.iteri (fun i d -> if k > d then p[i] <- p[i] + k - d else ())
                todo.Clear()
                todo[0] <- Queue supply
                g |> Array.iteri (fun i e -> if e > 0 then ind[i] <- i else ind[i] <- 0)
                iter 0 0

        supply |> Seq.iter (fun e -> dist[e] <- 0)
        todo[0] <- Queue supply
        iter 0 0

    let rex (sub: T) : Solution =

        failwith ""
