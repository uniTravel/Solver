namespace Solver.Linear


module Primal =

    let fpi (a: float[,]) m pj =
        [ 1..m ]
        |> List.fold
            (fun (pi, v) i ->
                match a[i, pj] with
                | p when p <= 0.0 -> pi, v
                | p ->
                    match a[i, 0] / p, v with
                    | k, -1.0 -> i, k
                    | k, _ when k < v -> i, k
                    | _ -> pi, v)
            (0, -1.0)

    type T = P of float[,] * int[] * int * int * int option

    let create pa ai =
        let pa = Array2D.copy pa
        let bl = Array2D.length1 pa - 1
        let n = Array2D.length2 pa - 1
        P(pa, [| 0..n |], n, bl, ai)

    let classic (P(a, l, n, m, _)) trans =
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
                match fpi a m pj with
                | 0, -1.0 -> raise UnboundedError
                | pi, _ ->
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    fold <| (pi, pj) :: r

        fold []

    let optimal (P(a, l, n, m, _)) trans =
        let rec fold r =
            match List.filter (fun j -> a[0, l[j]] < 0.0) [ m + 1 .. n ] with
            | [] -> r
            | l' ->
                match
                    l'
                    |> List.fold
                        (fun (pi, j, pj, v, o, ub) s ->
                            match fpi a m l[s] with
                            | 0, -1.0 -> pi, j, pj, v, o, ub
                            | i, v ->
                                match a[0, l[s]] * v with
                                | p when p < o -> i, s, l[s], v, p, ub &&& 0
                                | _ -> pi, j, pj, v, o, ub &&& 0)
                        (0, 0, 0, 0.0, 0.0, 1)
                with
                | 0, 0, 0, _, 0.0, 1 -> raise UnboundedError
                | pi, j, pj, _, _, _ ->
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    fold <| (pi, pj) :: r

        fold []

    let finish (P(a, l, n, m, _)) trans solution ni =
        List.filter (fun j -> a[0, l[j]] = 0.0) ni
        |> List.fold
            (fun acc j ->
                match fpi a m l[j] with
                | 0, -1.0 -> acc
                | pi, _ ->
                    let a = Array2D.copy a
                    let l = Array.copy l
                    let pj = l[j]
                    l[j] <- l[pi]
                    l[pi] <- pj
                    trans n m a pi pj
                    (a, l) :: acc)
            [ solution ]

    let solve pivot trans sim =
        let (P(a, l, n, m, ai)) = sim

        match ai with
        | None ->
            (pivot sim trans: (int * int) list) |> ignore
            finish sim trans (a, l) [ m + 1 .. n ]
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

                List.iter (fun s -> a[0, l[s]] <- a[0, l[s]] + c[l[s]]) n'
                pivot sim trans |> ignore
                finish sim trans (a, l) n'
            | _ -> raise NoSolutionError
