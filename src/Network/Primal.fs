namespace Solver.Network

open System.Collections.Generic


module Primal =

    type T =
        | Primal of
            max: int *
            cost: (int * int * int) list *
            capacity: IDictionary<int * int, int> *
            p: int[] *
            pred: (int * int)[] *
            depth: int[] *
            x: Dictionary<int * int, int> *
            u: Dictionary<int * int, int>

    let sc i j v (p: int[]) (u: Dictionary<int * int, int>) (candidate: Dictionary<int * int, int>) =
        match v + p[j] - p[i] with
        | 0 -> ()
        | r when r < 0 -> candidate[(i, j)] <- -r
        | r when u.ContainsKey(i, j) -> candidate[(i, j)] <- r
        | _ -> ()

    let prepare1 size cost p u (candidate: Dictionary<int * int, int>) =
        let rec apply cl =
            match cl with
            | (i, j, v) :: tail ->
                match candidate.Count with
                | s when s = size -> s
                | _ ->
                    sc i j v p u candidate
                    apply tail
            | [] -> candidate.Count

        apply cost

    let prepare2 cost p u candidate =
        let rec apply cl =
            match cl with
            | (i, j, v) :: tail ->
                sc i j v p u candidate
                apply tail
            | [] -> candidate.Count

        apply cost

    let pivot (Primal(max, cost, capacity, p, pred, depth, x, u)) (candidate: Dictionary<int * int, int>) =
        let rec joint leaf ci cj ia r =
            match leaf with
            | i, j when i = j ->
                let cycle = ci @ List.rev cj
                let oa, d, _ = cycle |> List.minBy (fun (_, v, _) -> v)

                match d with
                | d when d = max -> Unbounded
                | _ ->
                    cycle |> List.iter (fun (k, _, op) -> x[k] <- op x[k] d)
                    candidate.Remove ia |> ignore

                    if x[oa] > 0 then
                        u[oa] <- capacity[oa]

                    // TODO: update p pred depth

                    Feasible

            | i, j ->
                match depth[i], depth[j] with
                | di, dj when di > dj ->
                    match pred[i] with
                    | a when snd a = i ->
                        match capacity.ContainsKey a with
                        | true -> joint (fst a, j) ((a, capacity[a] - x[a], (+)) :: ci) cj ia r
                        | false -> joint (fst a, j) ((a, max, (+)) :: ci) cj ia r
                    | a -> joint (snd a, j) ((a, x[a], (-)) :: ci) cj ia r
                | _ ->
                    match pred[j] with
                    | a when fst a = j ->
                        match capacity.ContainsKey a with
                        | true -> joint (i, snd a) ci ((a, capacity[a] - x[a], (+)) :: cj) ia r
                        | false -> joint (i, snd a) ci ((a, max, (+)) :: cj) ia r
                    | a -> joint (i, fst a) ci ((a, x[a], (-)) :: cj) ia r

        match candidate |> Seq.maxBy (fun c -> c.Value) with
        | KeyValue(k, v) when u.ContainsKey k ->
            x[k] <- capacity[k]
            u.Remove k |> ignore
            joint k [ k, capacity[k], (-) ] [] k v
        | KeyValue(k, v) when capacity.ContainsKey k ->
            x[k] <- 0
            joint k [ k, capacity[k], (+) ] [] k v
        | KeyValue(k, v) ->
            x[k] <- 0
            joint k [ k, max, (+) ] [] k v

    let create sub =
        let n, cost, capacity = Subject.value sub
        let p = Array.zeroCreate<int> n.Length
        let pred = Array.zeroCreate<int * int> n.Length
        let depth = Array.zeroCreate<int> n.Length
        let x = Dictionary<int * int, int> n.Length
        let u = Dictionary<int * int, int>()
        let max = (n |> Array.takeWhile (fun v -> v >= 0) |> Array.sum) + 1
        let _, _, c = cost |> List.maxBy (fun (_, _, v) -> abs v)
        let m = (n.Length - 1) * c / 2 + 1
        pred[0] <- 0, 0
        depth[0] <- 0

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

        Primal(max, cost, capacity, p, pred, depth, x, u)

    let solve size sub =
        let (Primal(max, cost, capacity, p, pred, depth, x, u)) = sub

        let rec f (prepare, candidate: Dictionary<int * int, int>) =
            match candidate.Count with
            | 0 ->
                match prepare candidate with
                | 0 ->
                    match x |> Seq.exists (fun (KeyValue((i, j), v)) -> i = 0 || j = 0 && v > 0) with
                    | true -> Infeasible
                    | false -> Optimal(x, u)
                | _ -> f (prepare, candidate)
            | _ ->
                match pivot sub candidate with
                | Unbounded -> Unbounded
                | _ -> f (prepare, candidate)

        match size with
        | size when size < cost.Length ->
            let candidate = Dictionary<int * int, int> size
            prepare1 size cost p u, candidate
        | _ ->
            let candidate = Dictionary<int * int, int> cost.Length
            prepare2 cost p u, candidate
        |> f
