namespace Solver.Network

open System.Collections.Generic


module Primal =

    type T =
        | Primal of
            max: int *
            cost: IDictionary<int * int, int> *
            cap: IDictionary<int * int, int> *
            p: int[] *
            pred: (int * int)[] *
            depth: int[] *
            thread: int[] *
            x: Dictionary<int * int, int> *
            u: Dictionary<int * int, int>

    let pivot (Primal(max, cost, cap, p, pred, depth, thread, x, u)) (cand: Dictionary<int * int, int>) =
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
            let pruned = l[depth[oj] - depth[joint] .. depth[ii] - depth[joint] - 1]
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
                cand.Remove ia |> ignore

                match d with
                | d when d = max -> Unbounded
                | _ ->
                    cycle |> List.iter (fun (_, _, a, _, op) -> x[a] <- op x[a] d)

                    match x[oa] with
                    | 0 -> x.Remove oa |> ignore
                    | _ ->
                        u[oa] <- cap[oa]
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
                    | t, h as a when h = f ->
                        match cap.ContainsKey a with
                        | true -> joint (t, b) ((t, -h, a, cap[a] - x[a], (+)) :: cf) cb ia ii ij r
                        | false -> joint (t, b) ((t, -h, a, max, (+)) :: cf) cb ia ii ij r
                    | t, h as a -> joint (h, b) ((h, -t, a, x[a], (-)) :: cf) cb ia ii ij r
                | _ ->
                    match pred[b] with
                    | t, h as a when t = b ->
                        match cap.ContainsKey a with
                        | true -> joint (f, h) cf ((h, t, a, cap[a] - x[a], (+)) :: cb) ia ii ij r
                        | false -> joint (f, h) cf ((h, t, a, max, (+)) :: cb) ia ii ij r
                    | t, h as a -> joint (f, t) cf ((t, h, a, x[a], (-)) :: cb) ia ii ij r

        match cand |> Seq.maxBy (fun c -> abs c.Value) with
        | KeyValue((t, h as a), r) when u.ContainsKey a ->
            x[a] <- cap[a]
            u.Remove a |> ignore
            joint (h, t) [ h, 0, a, cap[a], (-) ] [] a h t r
        | KeyValue((t, h as a), r) when cap.ContainsKey a ->
            x[a] <- 0
            joint a [ fst a, 0, a, cap[a], (+) ] [] a t h r
        | KeyValue((t, h as a), r) ->
            x[a] <- 0
            joint a [ fst a, 0, a, max, (+) ] [] a t h r

    let create sub =
        let n, _, _, cost, cap = Subject.value sub
        let p = Array.zeroCreate<int> n.Length
        let pred = Array.create<int * int> n.Length (0, 0)
        let depth = Array.zeroCreate<int> n.Length
        let thread = Array.ofList [ 1 .. n.Length ]
        let x = Dictionary<int * int, int> n.Length
        let u = Dictionary<int * int, int>()
        let max = (n |> Array.filter (fun e -> e > 0) |> Array.sum) + 1
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

        Primal(max, cost, cap, p, pred, depth, thread, x, u)

    let solve size (Primal(max, cost, cap, p, pred, depth, thread, x, u) as sub) =
        let cl = cost |> Seq.map (fun (KeyValue(a, c)) -> a, c) |> Seq.toList

        let sc (t, h as a) c (cand: Dictionary<int * int, int>) =
            match u.ContainsKey a, c + p[h] - p[t] with
            | true, r when r > 0 -> cand[a] <- r
            | false, r when r < 0 -> cand[a] <- r
            | _ -> ()

        let prepare1 size (cand: Dictionary<int * int, int>) =
            let rec apply =
                function
                | (a, c) :: tail ->
                    match cand.Count with
                    | s when s = size -> s
                    | _ ->
                        sc a c cand
                        apply tail
                | [] -> cand.Count

            apply cl

        let prepare2 cand =
            let rec apply =
                function
                | (a, c) :: tail ->
                    sc a c cand
                    apply tail
                | [] -> cand.Count

            apply cl

        let rec run (prepare, cand: Dictionary<int * int, int>) =
            match cand.Count with
            | 0 ->
                match prepare cand with
                | 0 ->
                    match x |> Seq.find (fun (KeyValue((t, h), _)) -> t = 0 || h = 0) with
                    | KeyValue(_, v) when v > 0 -> Infeasible
                    | KeyValue(a, _) ->
                        x.Remove a |> ignore
                        u |> Seq.iter (fun (KeyValue(a, v)) -> x[a] <- v)
                        Optimal x
                | _ -> run (prepare, cand)
            | _ ->
                match pivot sub cand with
                | Unbounded -> Unbounded
                | _ ->
                    cand
                    |> Seq.iter (fun (KeyValue((t, h as a), r)) ->
                        match u.ContainsKey a, cost[a] + p[h] - p[t] with
                        | true, r when r > 0 -> cand[a] <- r
                        | false, r when r < 0 -> cand[a] <- r
                        | _ -> cand.Remove a |> ignore)

                    run (prepare, cand)

        match size with
        | size when size < cost.Count ->
            let cand = Dictionary<int * int, int> size
            prepare1 size, cand
        | _ ->
            let cand = Dictionary<int * int, int> cost.Count
            prepare2, cand
        |> run
