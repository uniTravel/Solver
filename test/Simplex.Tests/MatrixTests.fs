module Simplex.Tests.Matrix

open Expecto
open Solver.Simplex


[<Tests>]
let test1 =
    let cal len =
        let left = [| for i in 1 .. len -> 1.0 |]
        let right = [| for i in 1 .. len -> 2.0 |]
        Matrix.vv left right

    testList "行向量×列向量" [
        testCase "数组长度不规则，小于向量化计数" <| fun _ ->
              let result = cal 2
              Expect.equal result 4.0 "计算结果错误"
        testCase "数组长度不规则，大于向量化计数" <| fun _ ->
            let result = cal 5
            Expect.equal result 10.0 "计算结果错误"
        testCase "数组长度规则，等于向量化计数" <| fun _ ->
            let result = cal 4
            Expect.equal result 8.0 "计算结果错误"
        testCase "数组长度规则，数倍于向量化计数" <| fun _ ->
            let result = cal 8
            Expect.equal result 16.0 "计算结果错误"
    ] |> testLabel "Simplex"


[<Tests>]
let test2 =
    testList "矩阵计算" [ 
        testCase "行向量×矩阵" <| fun _ ->
              let left = [| for i in 1 .. 2 -> 1.0 |]
              let right = array2D [ [ 2.0; 2.0; 2.0 ]; [ 2.0; 2.0; 2.0 ] ]
              let result = Matrix.vm left right
              Expect.equal result [| 4.0; 4.0; 4.0 |] "计算结果错误"
        testCase "矩阵×列向量" <| fun _ ->
            let left = array2D [ [ 2.0; 2.0; 2.0 ]; [ 2.0; 2.0; 2.0 ] ]
            let right = [| for i in 1 .. 3 -> 1.0 |]
            let result = Matrix.mv left right
            Expect.equal result [| 6.0; 6.0 |] "计算结果错误"
        testCase "矩阵×矩阵" <| fun _ ->
            let left = array2D [ [ 2.0; 2.0; 2.0 ]; [ 2.0; 2.0; 2.0 ] ]
            let right = array2D [ [ 1.0; 1.0 ]; [ 1.0; 1.0 ]; [ 1.0; 1.0 ] ]
            let result = Matrix.mm left right
            let expectd = array2D [ [ 6.0; 6.0 ]; [ 6.0; 6.0 ] ]
            Expect.equal result expectd "计算结果错误"
    ] |> testLabel "Simplex"