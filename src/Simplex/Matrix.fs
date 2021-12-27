namespace Solver.Simplex

open System.Numerics


module Matrix =

    let chunkSize = Vector<float>.Count

    let vv (left: float []) (right: float []) =
        let mutable acc = 0.0
        let tail = left.Length - chunkSize

        let rec fast idx =
            if idx > tail then
                if idx - tail = chunkSize then
                    acc
                else
                    for i = idx to left.Length - 1 do
                        acc <- left[i] * right[i] + acc

                    acc
            else
                acc <-
                    Vector.Dot(Vector(left, idx), Vector(right, idx))
                    + acc

                fast <| idx + chunkSize

        fast 0

    let vm (left: float []) (right: float [,]) =
        let len = Array2D.length2 right
        let arr = Array.zeroCreate<float> len

        for i = 0 to len - 1 do
            arr[i] <- vv left right[*, i]

        arr

    let mv (left: float [,]) (right: float []) =
        let len = Array2D.length1 left
        let arr = Array.zeroCreate<float> len

        for i = 0 to len - 1 do
            arr[i] <- vv left[i, *] right

        arr

    let mm (left: float [,]) (right: float [,]) =
        let len1 = Array2D.length1 left
        let len2 = Array2D.length2 right
        let arr = Array2D.zeroCreate<float> len1 len2

        for i = 0 to len1 - 1 do
            for j = 0 to len2 - 1 do
                arr[i, j] <- vv left[i, *] right[*, j]

        arr

    let inv (matrix: float [,]) : float [,] = failwith ""