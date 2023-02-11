namespace Solver.Linear

open System.Collections.Generic


module Upper =

    let fpi (a: float[,]) (l: int[]) (u: IDictionary<int, float>) m pj =
        [ 1..m ]
        |> List.fold
            (fun (pi, v) i ->
                match a[i, pj] with
                | p when p = 0.0 -> pi, v
                | p when p < 0.0 ->
                    match u.ContainsKey l[i] with
                    | false -> pi, v
                    | _ ->
                        match (a[i, 0] - u[l[i]]) / p, v with
                        | k, v when k <= 0.0 -> pi, v
                        | k, -1.0 -> -i, k
                        | k, _ when k < v -> -i, k
                        | _ -> pi, v
                | p ->
                    match a[i, 0] / p, v with
                    | k, -1.0 -> i, k
                    | k, _ when k < v -> i, k
                    | _ -> pi, v)
            (match u.ContainsKey pj with
             | false -> (0, -1.0)
             | _ -> (0, u[pj]))

    let posi (a: float[,]) (l: int[]) (n: int) (m: int) j pi pj trans =
        l[j] <- l[pi]
        l[pi] <- pj
        trans n m a pi pj

    let zero (a: float[,]) (p: IDictionary<int, ref<bool>>) (m: int) pj v =
        [ 0..m ]
        |> List.iter (fun i ->
            a[i, 0] <- a[i, 0] - a[i, pj] * v
            a[i, pj] <- -a[i, pj])

        p[pj].Value <- true

    let nega (a: float[,]) (l: int[]) (p: IDictionary<int, ref<bool>>) (n: int) (m: int) j pi pj v trans =
        let pi = -pi
        l[j] <- l[pi]
        l[pi] <- pj
        a[pi, 0] <- a[pi, pj] * v
        a[pi, l[j]] <- -a[pi, l[j]]
        trans n m a pi pj
        p[l[j]].Value <- true

    type T = U of float[,] * int[] * IDictionary<int, float> * IDictionary<int, ref<bool>> * int * int * int option

    let create pa (u: IDictionary<int, float>) ai =
        let pa = Array2D.copy pa
        let bl = Array2D.length1 pa - 1
        let n = Array2D.length2 pa - 1
        let p = Seq.map (fun k -> k, ref false) u.Keys |> dict
        U(pa, [| 0..n |], u, p, n, bl, ai)

    let classic (U(a, l, u, p, n, m, _)) trans =
        let rec fold r =
            match
                [ m + 1 .. n ]
                |> List.fold
                    (fun (j, pj, v) s ->
                        match a[0, l[s]], v with
                        | p, v when p < v -> s, l[s], p
                        | _ -> j, pj, v)
                    (0, 0, 0.0)
            with
            | 0, 0, 0.0 -> r
            | j, pj, _ ->
                match fpi a l u m pj with
                | 0, -1.0 -> raise UnboundedError
                | pi, _ when pi > 0 ->
                    posi a l n m j pi pj trans
                    fold <| (pi, pj) :: r
                | 0, v ->
                    zero a p m pj v
                    fold <| (pj, pj) :: r
                | pi, v ->
                    nega a l p n m j pi pj v trans
                    fold <| (pi, pj) :: r

        fold []

    let optimal (U(a, l, u, p, n, m, _)) trans =
        let rec fold r =
            match List.filter (fun j -> a[0, l[j]] < 0.0) [ m + 1 .. n ] with
            | [] -> r
            | l' ->
                match
                    l'
                    |> List.fold
                        (fun (pi, j, pj, v, o, ub) s ->
                            match fpi a l u m l[s] with
                            | 0, -1.0 -> pi, j, pj, v, o, ub
                            | i, v ->
                                match a[0, l[s]] * v with
                                | p when p < o -> i, s, l[s], v, p, ub &&& 0
                                | _ -> pi, j, pj, v, o, ub &&& 0)
                        (0, 0, 0, 0.0, 0.0, 1)
                with
                | 0, 0, 0, _, 0.0, 1 -> raise UnboundedError
                | pi, j, pj, _, _, _ when pi > 0 ->
                    posi a l n m j pi pj trans
                    fold <| (pi, pj) :: r
                | 0, j, pj, v, _, _ ->
                    zero a p m pj v
                    fold <| (pj, pj) :: r
                | pi, j, pj, v, _, _ ->
                    nega a l p n m j pi pj v trans
                    fold <| (pi, pj) :: r

        fold []

    let finish (U(a, l, u, p, n, m, _)) trans solution ni =
        List.filter (fun j -> a[0, l[j]] = 0.0) ni
        |> List.fold
            (fun acc j ->
                match fpi a l u m l[j] with
                | 0, -1.0 -> acc
                | pi, v ->
                    let a = Array2D.copy a
                    let l = Array.copy l
                    let p = Seq.map2 (fun k v -> k, v) p.Keys p.Values |> dict

                    match (pi, l[j]) with
                    | pi, pj when pi > 0 -> posi a l n m j pi pj trans
                    | 0, pj -> zero a p m pj v
                    | pi, pj -> nega a l p n m j pi pj v trans

                    (a, l, p) :: acc)
            [ solution ]

    let solve pivot trans sim =
        let (U(a, l, u, p, n, m, ai)) = sim

        match ai with
        | None ->
            (pivot sim trans: (int * int) list) |> ignore
            finish sim trans (a, l, p) [ m + 1 .. n ]
        | Some ai ->
            let c = a[0, *]
            List.iter (fun j -> a[0, j] <- 0.0) [ m + 1 .. n ]
            List.iter (fun i -> a[0, 0] <- a[0, 0] - a[i, 0]) [ ai..m ]
            List.iter (fun j -> List.iter (fun i -> a[0, j] <- a[0, j] - a[i, j]) [ ai..m ]) [ m + 1 .. n ]
            pivot sim trans |> ignore

            match List.filter (fun i -> l[i] < ai || l[i] > m) [ m + 1 .. n ] with
            | n' when n'.Length = n + ai - m - m - 1 ->
                [ 1..m ]
                |> List.fold
                    (fun acc i ->
                        match l[i] with
                        | pj when c[pj] <> 0.0 -> (i, c[pj]) :: acc
                        | _ -> acc)
                    []
                |> List.iter (fun (i, k) ->
                    a[0, 0] <- a[0, 0] - a[i, 0] * k
                    List.iter (fun s -> a[0, l[s]] <- a[0, l[s]] - a[i, l[s]] * k) n')

                n'
                |> List.iter (fun s ->
                    match p[l[s]].Value with
                    | false -> a[0, l[s]] <- a[0, l[s]] + c[l[s]]
                    | _ ->
                        a[0, 0] <- a[0, 0] - u[l[s]] * c[l[s]]
                        a[0, l[s]] <- a[0, l[s]] - c[l[s]])

                pivot sim trans |> ignore
                finish sim trans (a, l, p) n'
            | _ -> raise NoSolutionError
