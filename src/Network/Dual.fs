namespace Solver.Network

open System
open System.Collections.Generic


module Dual =

    type T =
        | Dual of
            adj: int[][] *
            inv: int[][] *
            cost: IDictionary<int * int, int> *
            cap: IDictionary<int * int, int> *
            g: int[] *
            p: int[] *
            x: Dictionary<int * int, int> *
            aug: Stack<(int * int) * int * int * (int -> int -> int)>

    let augment sink (Dual(adj, inv, cost, cap, g, p, x, aug)) (ind: int[]) =
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
        i

    let price (Dual(adj, inv, cost, cap, g, p, x, aug)) ind (expand: Queue<int * int * bool>) =
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

    let create sub =
        let n, adj, inv, cost, cap = Subject.value sub

        if
            cap.Count <> cost.Count
            || (Seq.sort cap.Keys, Seq.sort cost.Keys) ||> Seq.exists2 (fun e1 e2 -> e1 <> e2)
        then
            invalidArg (nameof sub) $"Arcs must have upper bound."

        let g = Array.copy n
        let p = Array.zeroCreate<int> n.Length
        let x = Dictionary<int * int, int> cost.Count

        cost
        |> Seq.iter (function
            | KeyValue((i, j as k), v) when v < 0 ->
                x[k] <- cap[k]
                g[i] <- g[i] - x[k]
                g[j] <- g[j] + x[k]
            | KeyValue(k, _) -> x[k] <- 0)

        let aug = Stack<(int * int) * int * int * (int -> int -> int)> n.Length
        Dual(adj, inv, cost, cap, g, p, x, aug)

    let pd (Dual(adj, inv, cost, cap, g, p, x, aug) as sub) =
        let mutable sink = 0
        let indicator = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = indicator |> Array.filter (fun e -> e <> 0) |> HashSet
        let todo = Queue<int> g.Length
        let expand = Queue<int * int * bool> g.Length
        supply |> Seq.iter (fun e -> todo.Enqueue e)

        let check i j (ind: int[]) =
            match sink, ind[j] with
            | 0, 0 ->
                ind[j] <- i
                if g[j] = 0 then todo.Enqueue j else sink <- j
            | _ -> ()

        let rec iter (ind: int[]) =
            match supply.Count, sink with
            | 0, _ -> Optimal x
            | _, 0 ->
                match todo.Count with
                | 0 ->
                    match price sub ind expand with
                    | Int32.MaxValue -> Infeasible
                    | d ->
                        ind |> Array.iteri (fun i e -> if e <> 0 then p[i] <- p[i] + d else ())

                        while expand.Count <> 0 do
                            match expand.Dequeue() with
                            | i, j, true -> check i j ind
                            | i, j, false -> check -i j ind

                        iter ind
                | _ ->
                    let i = todo.Dequeue()

                    adj[i]
                    |> Array.iter (fun j ->
                        match p[j] + cost[(i, j)] with
                        | v when v = p[i] && x[(i, j)] <> cap[(i, j)] -> check i j ind
                        | _ -> ())

                    inv[i]
                    |> Array.iter (fun j ->
                        match p[j] - cost[(j, i)] with
                        | v when v = p[i] && x[(j, i)] <> 0 -> check -i j ind
                        | _ -> ())

                    iter ind
            | _, s ->
                match augment s sub ind with
                | i when g[i] = 0 ->
                    supply.Remove i |> ignore
                    indicator[i] <- 0
                | _ -> ()

                todo.Clear()
                supply |> Seq.iter (fun e -> todo.Enqueue e)
                sink <- 0
                Array.copy indicator |> iter

        Array.copy indicator |> iter

    let ssp (Dual(adj, inv, cost, cap, g, p, x, aug) as sub) =
        let indicator = Array.init g.Length <| fun i -> if g[i] > 0 then i else 0
        let supply = indicator |> Array.filter (fun e -> e <> 0) |> HashSet
        let distance = Array.create g.Length Int32.MaxValue
        let todo = Dictionary<int, Queue<int>>()
        supply |> Seq.iter (fun e -> distance[e] <- 0)

        let check i j o (ind: int[]) (dist: int[]) =
            function
            | d when d < dist[j] ->
                dist[j] <- d
                ind[j] <- if o then i else -i
                if todo.ContainsKey d then () else todo[d] <- Queue<int>()
                todo[d].Enqueue j
            | _ -> ()

        let scan k (ind: int[]) (dist: int[]) =
            match todo[k].Dequeue() with
            | i when dist[i] <> k -> 0
            | i when g[i] < 0 -> i
            | i ->
                adj[i]
                |> Array.iter (fun j ->
                    match x[(i, j)] with
                    | x when x = cap[(i, j)] -> ()
                    | _ ->
                        match p[j] + cost[(i, j)] - p[i] with
                        | 0 -> check i j true ind dist dist[i]
                        | r -> check i j true ind dist <| dist[i] + r)

                inv[i]
                |> Array.iter (fun j ->
                    match x[(j, i)] with
                    | 0 -> ()
                    | _ ->
                        match p[j] - cost[(j, i)] - p[i] with
                        | 0 -> check i j false ind dist dist[i]
                        | r -> check i j false ind dist <| dist[i] + r)

                0

        let rec iter k (ind: int[]) (dist: int[]) sink =
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
                        iter k ind dist 0
                | _ -> iter k ind dist <| scan k ind dist
            | _, s ->
                match augment s sub ind with
                | i when g[i] = 0 ->
                    supply.Remove i |> ignore
                    indicator[i] <- 0
                | _ -> ()

                dist |> Array.iteri (fun i d -> if k > d then p[i] <- p[i] + k - d else ())
                todo.Clear()
                todo[0] <- Queue supply
                iter 0 (Array.copy indicator) (Array.copy distance) 0

        todo[0] <- Queue supply
        iter 0 (Array.copy indicator) (Array.copy distance) 0

    let rex (Dual(adj, inv, cost, cap, g, p, x, aug) as sub) =
        let mutable sink = 0
        let mutable q = 0
        let ind = Array.zeroCreate g.Length
        let supply = Queue<int> g.Length
        let todo = Queue<int> g.Length
        let expand = Queue<int * int * bool> g.Length
        g |> Array.iteri (fun i e -> if e > 0 then supply.Enqueue i else ())

        let init i =
            q <-
                g[i]
                - (adj[i]
                   |> Array.sumBy (fun j ->
                       match p[j] + cost[(i, j)] with
                       | v when v = p[i] -> cap[(i, j)] - x[(i, j)]
                       | _ -> 0))
                - (inv[i]
                   |> Array.sumBy (fun j ->
                       match p[j] - cost[(j, i)] with
                       | v when v = p[i] -> x[(j, i)]
                       | _ -> 0))

        let update () =
            q <-
                q
                - (expand
                   |> Seq.sumBy (fun (i, j, o) -> if o then cap[(i, j)] - x[(i, j)] else x[(j, i)]))

        let append i =
            q <-
                q
                + g[i]
                + (adj[i]
                   |> Array.filter (fun j -> p[j] + cost[(i, j)] = p[i])
                   |> Array.sumBy (fun j -> if ind[j] = 0 then x[(i, j)] - cap[(i, j)] else x[(i, j)]))
                - (inv[i]
                   |> Array.filter (fun j -> p[j] - cost[(j, i)] = p[i])
                   |> Array.sumBy (fun j -> if ind[j] = 0 then x[(j, i)] else x[(j, i)] - cap[(j, i)]))

        let check i j o =
            match sink, ind[j] with
            | 0, 0 ->
                append i
                if g[j] < 0 then sink <- j else todo.Enqueue j
                if o then ind[j] <- i else ind[j] <- -i
            | _ -> ()

        let clear () =
            todo.Clear()
            sink <- 0
            Array.Clear ind

        let reset t =
            todo.Enqueue t
            ind[t] <- t
            init t
            t

        let rec iter t =
            let apply () =
                match price sub ind expand with
                | Int32.MaxValue -> Infeasible
                | d ->
                    ind |> Array.iteri (fun i e -> if e <> 0 then p[i] <- p[i] + d else ())
                    update ()

                    match q with
                    | q' when q' > 0 -> expand.Clear()
                    | _ ->
                        while expand.Count <> 0 do
                            match expand.Dequeue() with
                            | i, j, true -> check i j true
                            | i, j, false -> check i j false

                    iter t

            match sink, todo.Count with
            | 0, 0 -> apply ()
            | 0, _ ->
                match q with
                | q' when q' > 0 ->
                    ind
                    |> Array.iteri (fun i e ->
                        match e with
                        | 0 -> ()
                        | _ ->
                            adj[i]
                            |> Array.iter (fun j ->
                                match ind[j], p[j] + cost[(i, j)] with
                                | 0, v when v = p[i] && x[(i, j)] <> cap[(i, j)] ->
                                    let v = cap[(i, j)] - x[(i, j)]
                                    let gj = g[j]
                                    x[(i, j)] <- cap[(i, j)]
                                    g[i] <- g[i] - v
                                    g[j] <- g[j] + v
                                    if gj <= 0 && g[j] > 0 then supply.Enqueue j else ()
                                    q <- q - v
                                | _ -> ())

                            inv[i]
                            |> Array.iter (fun j ->
                                match ind[j], p[j] - cost[(j, i)] with
                                | 0, v when v = p[i] && x[(j, i)] <> 0 ->
                                    let v = x[(j, i)]
                                    let gj = g[j]
                                    x[(j, i)] <- 0
                                    g[i] <- g[i] - v
                                    g[j] <- g[j] + v
                                    if gj <= 0 && g[j] > 0 then supply.Enqueue j else ()
                                    q <- q - v
                                | _ -> ()))

                    if g[t] = 0 then iter t else apply ()
                | _ ->
                    let i = todo.Dequeue()

                    adj[i]
                    |> Array.iter (fun j ->
                        match p[j] + cost[(i, j)] with
                        | v when v = p[i] && x[(i, j)] <> cap[(i, j)] -> check i j true
                        | _ -> ())

                    inv[i]
                    |> Array.iter (fun j ->
                        match p[j] - cost[(j, i)] with
                        | v when v = p[i] && x[(j, i)] <> 0 -> check i j false
                        | _ -> ())

                    iter t
            | s, _ ->
                match g[augment s sub ind] with
                | 0 ->
                    match supply.Count with
                    | 0 -> Optimal x
                    | _ ->
                        clear ()
                        supply.Dequeue() |> reset |> iter
                | _ ->
                    clear ()
                    reset t |> iter

        supply.Dequeue() |> reset |> iter
