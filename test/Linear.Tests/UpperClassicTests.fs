module Solver.Linear.Tests.Upper.Classic

open Expecto
open Solver.Linear


let private validate (a: float[,]) (l: int[]) pe le =
    Array2D.iteri (fun i j e -> Expect.floatClose Accuracy.veryHigh a[i, j] e $"位置 {i},{j} 计算错误。") pe
    Expect.equal l le "索引数组错误。"

let private solve = Upper.solve Upper.classic Row.trans
let private asolve = Upper.solve Upper.classic Row.atrans

[<Tests>]
let test1 =
    let p: float[,] = array2D [ [ 20; 0; 0; -2 ]; [ 12; 1; 0; 4 ]; [ 4; 0; 1; -2 ] ]

    let pe: float[,] = array2D [ [ 22; 0; 1; 0 ]; [ 8; 1; -2; 0 ]; [ 1; 0; 0.5; 1 ] ]

    let u = dict [ 1, 15.0; 2, 6.0; 3, 4.0 ]
    let le = [| 0; 1; 3; 2 |]

    testList
        "带上界约束的问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Upper.create p u None
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l, p = result[0]
              Expect.sequenceEqual p.Values [ ref false; ref true; ref false ] "上界解指示器错误。"
              validate a l pe le
          testCase "异步算法"
          <| fun _ ->
              let sim = Upper.create p u None
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l, p = result[0]
              Expect.sequenceEqual p.Values [ ref false; ref true; ref false ] "上界解指示器错误。"
              validate a l pe le ]
    |> testLabel "Linear.Upper.Classic"

[<Tests>]
let test2 =
    let p: float[,] =
        array2D [ [ 0; 0; 0; -2; -1; -2 ]; [ 12; 1; 0; 4; 1; 0 ]; [ 4; 0; 1; -2; 0; 1 ] ]

    let pe: float[,] =
        array2D [ [ 22; 1; 1; 0; 0; 1 ]; [ 1; 0; -0.5; 1; 0; 0.5 ]; [ 8; 1; 2; 0; 1; -2 ] ]

    let u = dict [ 3, 4.0; 4, 15.0; 5, 6.0 ]
    let le = [| 0; 3; 4; 1; 2; 5 |]

    testList
        "带上界约束的人工问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Upper.create p u <| Some 1
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l, p = result[0]
              Expect.sequenceEqual p.Values [ ref false; ref false; ref true ] "上界解指示器错误。"
              validate a l pe le
          testCase "异步算法"
          <| fun _ ->
              let sim = Upper.create p u <| Some 1
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l, p = result[0]
              Expect.sequenceEqual p.Values [ ref false; ref false; ref true ] "上界解指示器错误。"
              validate a l pe le ]
    |> testLabel "Linear.Upper.Classic"
