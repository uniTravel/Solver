namespace Solver.Linear


module Row =

    let row n (a: float[,]) pi pj i =
        match a[i, pj] with
        | 1.0 -> List.iter (fun j -> a[i, j] <- a[i, j] - a[pi, j]) [ 0..n ]
        | k -> List.iter (fun j -> a[i, j] <- a[i, j] - a[pi, j] * k) [ 0..n ]

    let trans n m (a: float[,]) pi pj =
        match a[pi, pj] with
        | 1.0 -> ()
        | k -> List.iter (fun j -> a[pi, j] <- a[pi, j] / k) [ 0..n ]

        List.iter
            (fun i ->
                match i with
                | i when i = pi -> ()
                | i when a[i, pj] = 0.0 -> ()
                | _ -> row n a pi pj i)
            [ 0..m ]

    let atrans n m (a: float[,]) pi pj =
        match a[pi, pj] with
        | 1.0 -> ()
        | k -> List.iter (fun j -> a[pi, j] <- a[pi, j] / k) [ 0..n ]

        List.fold
            (fun acc i ->
                match i with
                | i when i = pi -> acc
                | i when a[i, pj] = 0.0 -> acc
                | _ -> async { row n a pi pj i } :: acc)
            []
            [ 0..m ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
