namespace Solver.Simplex


module Dual =

    let fpj (a: float[,]) (l: int[]) n m pi =
        [ m + 1 .. n ]
        |> List.fold
            (fun (j, pj, v) s ->
                match a[pi, l[s]] with
                | p when p >= 0.0 -> j, pj, v
                | p ->
                    match a[0, l[s]] / p, v with
                    | k, 1.0 -> s, l[s], k
                    | k, _ when k > v -> s, l[s], k
                    | _ -> j, pj, v)
            (0, 0, 1.0)

    type T = D of float[,] * int[] * int * int * int option

    let create da ai =
        let da = Array2D.copy da
        let bl = Array2D.length1 da - 1
        let n = Array2D.length2 da - 1
        D(da, [| 0..n |], n, bl, ai)

    let ofPrimal pa ai =
        let bl = Array2D.length1 pa - 1
        let n = Array2D.length2 pa - 1
        let cl = n - bl
        let da = Array2D.create (cl + 1) (n + 1) 0.0
        List.iter (fun i -> da[i, 0] <- pa[0, i + bl]) [ 1..cl ]
        List.iter (fun j -> da[0, j] <- pa[j - cl, 0]) [ cl + 1 .. n ]
        List.iter (fun i -> da[i, i] <- 1.0) [ 1..cl ]
        List.iter (fun i -> List.iter (fun j -> da[i, j] <- -pa[j - cl, bl + i]) [ cl + 1 .. n ]) [ 1..cl ]

        match ai with
        | None -> D(da, [| 0..n |], n, cl, None)
        | Some ai -> D(da, [| 0..n |], n, cl, Some(ai + cl))

    let classic (D (a, l, n, m, _)) trans =
        let rec fold r =
            match
                [ 1..m ]
                |> List.fold
                    (fun (pi, v) i ->
                        match a[i, 0], v with
                        | p, v when p < v -> i, p
                        | _ -> pi, v)
                    (0, 0.0)
            with
            | 0, 0.0 -> r
            | pi, _ ->
                match fpj a l n m pi with
                | 0, 0, 1.0 -> raise UnboundedError
                | j, pj, _ ->
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    fold <| (pi, pj) :: r

        fold []

    let optimal (D (a, l, n, m, _)) trans =
        let rec fold r =
            match List.filter (fun i -> a[i, 0] < 0.0) [ 1..m ] with
            | [] -> r
            | l' ->
                match
                    l'
                    |> List.fold
                        (fun (pi, j, pj, v, ub) i ->
                            match fpj a l n m i with
                            | 0, 0, 1.0 -> pi, j, pj, v, ub
                            | jj, jp, _ ->
                                match a[i, 0] * a[0, jp] / a[i, jp] with
                                | p when p > v -> i, jj, jp, p, ub &&& 0
                                | _ -> pi, j, pj, v, ub &&& 0)
                        (0, 0, 0, 0.0, 1)
                with
                | 0, 0, 0, 0.0, 1 -> raise UnboundedError
                | pi, j, pj, _, _ ->
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    fold <| (pi, pj) :: r

        fold []

    let finish (D (a, l, n, m, _)) trans solution bi =
        List.filter (fun i -> a[i, 0] = 0.0) bi
        |> List.fold
            (fun acc pi ->
                match fpj a l n m pi with
                | 0, 0, 1.0 -> acc
                | j, pj, _ ->
                    let a = Array2D.copy a
                    let l = Array.copy l
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    (a, l) :: acc)
            [ solution ]

    let solve pivot trans sim =
        let (D (a, l, n, m, ai)) = sim

        match ai with
        | None ->
            (pivot sim trans: (int * int) list) |> ignore
            finish sim trans (a, l) [ 1..m ]
        | Some ai ->
            let c = Array.zeroCreate (n + 1)
            Array.iteri (fun i e -> c[i] <- e) a[*, 0]
            List.iter (fun i -> a[i, 0] <- 0.0) [ 1..m ]
            List.iter (fun i -> List.iter (fun j -> a[i, 0] <- a[i, 0] + a[i, j]) [ ai..n ]) [ 0..m ]
            pivot sim trans |> ignore

            match List.filter (fun j -> l[j] < ai) [ 1..m ] with
            | b' when b'.Length = m + ai - n - 1 ->
                List.fold
                    (fun acc j ->
                        match l[j] with
                        | pj when c[pj] <> 0.0 -> (pj, c[pj]) :: acc
                        | _ -> acc)
                    []
                    [ m + 1 .. n ]
                |> List.iter (fun (j, k) ->
                    a[0, 0] <- a[0, 0] + a[0, j] * k
                    List.iter (fun s -> a[s, 0] <- a[s, 0] + a[s, j] * k) b')

                List.iter (fun s -> a[s, 0] <- a[s, 0] + c[l[s]]) b'
                pivot sim trans |> ignore
                finish sim trans (a, l) b'
            | _ -> raise NoSolutionError
