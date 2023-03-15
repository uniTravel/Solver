namespace Solver.Network

open System.Collections.Generic


module Primal =

    type T =
        | Primal of
            max: int *
            cost: IDictionary<int * int, int> *
            capacity: IDictionary<int * int, int> *
            p: int[] *
            pred: (int * int)[] *
            depth: int[] *
            thread: int[] *
            x: Dictionary<int * int, int> *
            u: Dictionary<int * int, int>

    let pivot (Primal(max, cost, capacity, p, pred, depth, thread, x, u)) (candidate: Dictionary<int * int, int>) =
        let subtree root =
            let rec apply current tree =
                match thread[current] with
                | next when depth[next] <= depth[root] -> root, tree
                | next -> apply next <| next :: tree

            apply root [ root ]

        let prune root s e =
            let rec apply current tree =
                match thread[current] with
                | next when depth[next] <= depth[root] -> root, tree
                | next when next = s ->
                    thread[current] <- thread[e]
                    apply e tree
                | next -> apply next <| next :: tree

            apply root [ root ]

        let update oi oj ii ij joint r (l: (int * int * (int * int) * int * (int -> int -> int)) list) =
            let _, origin = subtree oj
            let s = depth[oj] - depth[joint]
            let e = depth[ii] - depth[joint] - 1
            let pruned = l[s..e]
            let junction, tree = subtree ij
            origin |> List.iter (fun n -> p[n] <- p[n] + r)
            prune oi oj origin.Head |> ignore
            let next = thread[tree.Head]

            (pruned, subtree ii)
            ||> List.scanBack (fun (i, _, a, _, _) (v, tree) ->
                pred[i] <- a
                prune i v tree.Head)
            |> List.rev
            |> List.fold
                (fun (s, e) (v, tree) ->
                    match depth[s] + 1 - depth[v] with
                    | 0 -> ()
                    | d -> tree |> List.iter (fun i -> depth[i] <- depth[i] + d)

                    thread[e] <- v
                    v, tree.Head)
                (junction, tree.Head)
            |> (fun (_, e) -> thread[e] <- next)

        let rec joint leaf cf cb ia ii ij r =
            match leaf with
            | f, b when f = b ->
                let cycle = cf @ List.rev cb
                let oi, oj, oa, d, _ = cycle |> List.minBy (fun (_, _, _, d, _) -> d)
                candidate.Remove ia |> ignore

                match d with
                | d when d = max -> Unbounded
                | _ ->
                    cycle |> List.iter (fun (_, _, a, _, op) -> x[a] <- op x[a] d)

                    match x[oa] with
                    | 0 -> x.Remove oa |> ignore
                    | _ ->
                        u[oa] <- capacity[oa]
                        x.Remove oa |> ignore

                    match oj with
                    | 0 -> ()
                    | j when j < 0 ->
                        pred[ii] <- ia
                        update oi -j ii ij f r cf
                    | j ->
                        pred[ij] <- ia
                        update oi j ij ii b -r cb

                    Feasible

            | f, b ->
                match depth[f], depth[b] with
                | df, db when df > db ->
                    match pred[f] with
                    | t, h when h = f ->
                        match capacity.ContainsKey(t, h) with
                        | true -> joint (t, b) ((t, -h, (t, h), capacity[(t, h)] - x[(t, h)], (+)) :: cf) cb ia ii ij r
                        | false -> joint (t, b) ((t, -h, (t, h), max, (+)) :: cf) cb ia ii ij r
                    | t, h -> joint (h, b) ((h, -t, (t, h), x[(t, h)], (-)) :: cf) cb ia ii ij r
                | _ ->
                    match pred[b] with
                    | t, h when t = b ->
                        match capacity.ContainsKey(t, h) with
                        | true -> joint (f, h) cf ((h, t, (t, h), capacity[(t, h)] - x[(t, h)], (+)) :: cb) ia ii ij r
                        | false -> joint (f, h) cf ((h, t, (t, h), max, (+)) :: cb) ia ii ij r
                    | t, h -> joint (f, t) cf ((t, h, (t, h), x[(t, h)], (-)) :: cb) ia ii ij r

        match candidate |> Seq.maxBy (fun c -> abs c.Value) with
        | KeyValue(a, r) when u.ContainsKey a ->
            let t, h = a
            x[a] <- capacity[a]
            u.Remove a |> ignore
            joint (h, t) [ h, 0, a, capacity[a], (-) ] [] a h t r
        | KeyValue(a, r) when capacity.ContainsKey a ->
            let t, h = a
            x[a] <- 0
            joint a [ fst a, 0, a, capacity[a], (+) ] [] a t h r
        | KeyValue(a, r) ->
            let t, h = a
            x[a] <- 0
            joint a [ fst a, 0, a, max, (+) ] [] a t h r

    let create sub =
        let n, cost, capacity = Subject.value sub
        let p = Array.zeroCreate<int> n.Length
        let pred = Array.create<int * int> n.Length (0, 0)
        let depth = Array.zeroCreate<int> n.Length
        let thread = Array.ofList [ 1 .. n.Length ]
        let x = Dictionary<int * int, int>(n.Length - 1)
        let u = Dictionary<int * int, int>()
        let max = (n |> Array.takeWhile (fun v -> v >= 0) |> Array.sum) + 1
        let (KeyValue(_, c)) = cost |> Seq.maxBy (fun (KeyValue(_, c)) -> abs c)
        let m = (n.Length - 1) * c / 2 + 1
        thread[n.Length - 1] <- 0

        [ 1 .. n.Length - 1 ]
        |> List.iter (fun i ->
            match n[i] with
            | s when s <= 0 ->
                p[i] <- -m
                pred[i] <- 0, i
                depth[i] <- 1
                x[(0, i)] <- -s
            | s ->
                p[i] <- m
                pred[i] <- i, 0
                depth[i] <- 1
                x[(i, 0)] <- s)

        Primal(max, cost, capacity, p, pred, depth, thread, x, u)

    let solve size sub =
        let (Primal(max, cost, capacity, p, pred, depth, thread, x, u)) = sub
        let cl = cost |> Seq.map (fun (KeyValue((t, h), c)) -> t, h, c) |> Seq.toList

        let sc t h c (candidate: Dictionary<int * int, int>) =
            match u.ContainsKey(t, h), c + p[h] - p[t] with
            | true, r when r > 0 -> candidate[(t, h)] <- r
            | false, r when r < 0 -> candidate[(t, h)] <- r
            | _ -> ()

        let prepare1 size (candidate: Dictionary<int * int, int>) =
            let rec apply =
                function
                | (t, h, c) :: tail ->
                    match candidate.Count with
                    | s when s = size -> s
                    | _ ->
                        sc t h c candidate
                        apply tail
                | [] -> candidate.Count

            apply cl

        let prepare2 candidate =
            let rec apply =
                function
                | (t, h, c) :: tail ->
                    sc t h c candidate
                    apply tail
                | [] -> candidate.Count

            apply cl

        let rec run (prepare, candidate: Dictionary<int * int, int>) =
            match candidate.Count with
            | 0 ->
                match prepare candidate with
                | 0 ->
                    match x |> Seq.exists (fun (KeyValue((t, h), v)) -> (t = 0 || h = 0) && v > 0) with
                    | true -> Infeasible
                    | false -> Optimal(x, u)
                | _ -> run (prepare, candidate)
            | _ ->
                match pivot sub candidate with
                | Unbounded -> Unbounded
                | _ ->
                    candidate
                    |> Seq.iter (fun (KeyValue((t, h), r)) ->
                        match u.ContainsKey(t, h), cost[(t, h)] + p[h] - p[t] with
                        | true, r when r > 0 -> candidate[(t, h)] <- r
                        | false, r when r < 0 -> candidate[(t, h)] <- r
                        | _ -> candidate.Remove((t, h)) |> ignore)

                    run (prepare, candidate)

        match size with
        | size when size < cost.Count ->
            let candidate = Dictionary<int * int, int> size
            prepare1 size, candidate
        | _ ->
            let candidate = Dictionary<int * int, int> cost.Count
            prepare2, candidate
        |> run
