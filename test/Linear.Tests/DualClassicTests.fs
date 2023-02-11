module Solver.Linear.Tests.Dual.Classic

open Expecto
open Solver.Linear


let private validate (a: float[,]) (l: int[]) de le =
    Array2D.iteri (fun i j e -> Expect.floatClose Accuracy.veryHigh a[i, j] e $"位置 {i},{j} 计算错误。") de
    Expect.equal l le "索引数组错误。"

let private solve = Dual.solve Dual.classic Row.trans
let private asolve = Dual.solve Dual.classic Row.atrans

[<Tests>]
let test1 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; -3; -5 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 12; 0; 1; 0; 0; 2 ]
              [ 18; 0; 0; 1; 3; 2 ] ]

    let de: float[,] =
        array2D
            [ [ -36; 2; 6; 2; 0; 0 ]
              [ 1; -1.0 / 3.0; 0; 1.0 / 3.0; 0; 1 ]
              [ 1.5; 1.0 / 3.0; -0.5; -1.0 / 3.0; 1; 0 ] ]

    let le = [| 0; 5; 4; 3; 2; 1 |]

    testList
        "一般的标准问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le ]
    |> testLabel "Linear.Dual.Classic"

[<Tests>]
let test2 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; -3; -2 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 12; 0; 1; 0; 0; 2 ]
              [ 18; 0; 0; 1; 3; 2 ] ]

    let de1: float[,] =
        array2D [ [ -18; 4; 3; 0; 6; 0 ]; [ 0; -1; 1.5; 1; -3; 0 ]; [ 1; 0; -0.5; 0; 1; 1 ] ]

    let de2: float[,] =
        array2D
            [ [ -18; 2; 6; 2; 0; 0 ]
              [ 0; 1.0 / 3.0; -0.5; -1.0 / 3.0; 1; 0 ]
              [ 1; -1.0 / 3.0; 0; 1.0 / 3.0; 0; 1 ] ]

    let le1 = [| 0; 3; 5; 1; 4; 2 |]
    let le2 = [| 0; 4; 5; 1; 3; 2 |]

    testList
        "多最优解的标准问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let result = solve sim
              Expect.hasLength result 2 "最优解数量错误。"
              let a, l = result[1]
              validate a l de1 le1
              let a, l = result[0]
              validate a l de2 le2
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let result = asolve sim
              Expect.hasLength result 2 "最优解数量错误。"
              let a, l = result[1]
              validate a l de1 le1
              let a, l = result[0]
              validate a l de2 le2 ]
    |> testLabel "Linear.Dual.Classic"

[<Tests>]
let test3 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; -3; -5 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 12; 0; 1; 0; 0; 0 ]
              [ 18; 0; 0; 1; 3; 0 ] ]

    testList
        "标准问题无界"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let f = fun _ -> solve sim |> ignore
              Expect.throwsT<UnboundedError> f "异常类型错误。"
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p None
              let f = fun _ -> asolve sim |> ignore
              Expect.throwsT<UnboundedError> f "异常类型错误。" ]
    |> testLabel "Linear.Dual.Classic"

[<Tests>]
let test4 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; 0.4; 0.5; 0 ]
              [ 2.7; 1; 0; 0; 0.3; 0.1; 0 ]
              [ 6; 0; 1; 0; 0.5; 0.5; 0 ]
              [ 6; 0; 0; 1; 0.6; 0.4; -1 ] ]

    let de: float[,] =
        array2D
            [ [ 5.25; 7.5; 4.5; 0.3; 0; 0; 0 ]
              [ 0.5; -5; 5; -1; 1; 0; 0 ]
              [ 1; 0; 0; 1; 0; 0; 1 ]
              [ 1; 1; -3; -0.6; 0; 1; 0 ] ]

    let le = [| 0; 4; 6; 5; 1; 3; 2 |]

    testList
        "一般的人工问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 2
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 2
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le ]
    |> testLabel "Linear.Dual.Classic"

[<Tests>]
let test5 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; 0; 0; -2; -1; -2 ]
              [ 4; 1; 0; 0; 0; 0; 1; 0; 0 ]
              [ 15; 0; 1; 0; 0; 0; 0; 1; 0 ]
              [ 6; 0; 0; 1; 0; 0; 0; 0; 1 ]
              [ 12; 0; 0; 0; 1; 0; 4; 1; 0 ]
              [ 4; 0; 0; 0; 0; 1; -2; 0; 1 ] ]

    let de: float[,] =
        array2D
            [ [ -22; 1; 8; 6; 3; 7; 0; 0; 0 ]
              [ 1; 0; -1; 0; 0; 1; 0; 1; 0 ]
              [ 1; 0.5; -2; 0; -0.5; 2; 0; 0; 1 ]
              [ 1; -0.5; 2; -1; 0.5; -2; 1; 0; 0 ] ]

    let le = [| 0; 7; 8; 6; 4; 5; 3; 1; 2 |]

    testList
        "带上界约束的人工问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 4
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 4
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l de le ]
    |> testLabel "Linear.Dual.Classic"

[<Tests>]
let test6 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; 0.4; 0.5; 0 ]
              [ 1.8; 1; 0; 0; 0.3; 0.1; 0 ]
              [ 6; 0; 1; 0; 0.5; 0.5; 0 ]
              [ 6; 0; 0; 1; 0.6; 0.4; -1 ] ]

    testList
        "人工问题无解"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 2
              let f = fun _ -> solve sim |> ignore
              Expect.throwsT<NoSolutionError> f "异常类型错误。"
          testCase "异步算法"
          <| fun _ ->
              let sim = Dual.ofPrimal p <| Some 2
              let f = fun _ -> asolve sim |> ignore
              Expect.throwsT<NoSolutionError> f "异常类型错误。" ]
    |> testLabel "Linear.Dual.Classic"
