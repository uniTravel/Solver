module Solver.Linear.Tests.Primal.Optimal

open Expecto
open Solver.Linear


let private validate (a: float[,]) (l: int[]) pe le =
    Array2D.iteri (fun i j e -> Expect.floatClose Accuracy.veryHigh a[i, j] e $"位置 {i},{j} 计算错误。") pe
    Expect.equal l le "索引数组错误。"

let private solve = Primal.solve Primal.optimal Row.trans
let private asolve = Primal.solve Primal.optimal Row.atrans

[<Tests>]
let test1 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; -3; -5 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 12; 0; 1; 0; 0; 2 ]
              [ 18; 0; 0; 1; 3; 2 ] ]

    let pe: float[,] =
        array2D
            [ [ 36; 0; 1.5; 1; 0; 0 ]
              [ 2; 1; 1.0 / 3.0; -1.0 / 3.0; 0; 0 ]
              [ 6; 0; 0.5; 0; 0; 1 ]
              [ 2; 0; -1.0 / 3.0; 1.0 / 3.0; 1; 0 ] ]

    let le = [| 0; 1; 5; 4; 3; 2 |]

    testList
        "一般的标准问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Primal.create p None
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p None
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le ]
    |> testLabel "Linear.Primal.Optimal"

[<Tests>]
let test2 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; -3; -2 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 12; 0; 1; 0; 0; 2 ]
              [ 18; 0; 0; 1; 3; 2 ] ]

    let pe1: float[,] =
        array2D
            [ [ 18; 0; 0; 1; 0; 0 ]
              [ 4; 1; 0; 0; 1; 0 ]
              [ 6; 3; 1; -1; 0; 0 ]
              [ 3; -1.5; 0; 0.5; 0; 1 ] ]

    let pe2: float[,] =
        array2D
            [ [ 18; 0; 0; 1; 0; 0 ]
              [ 2; 0; -1.0 / 3.0; 1.0 / 3.0; 1; 0 ]
              [ 2; 1; 1.0 / 3.0; -1.0 / 3.0; 0; 0 ]
              [ 6; 0; 0.5; 0; 0; 1 ] ]

    let le1 = [| 0; 4; 2; 5; 1; 3 |]
    let le2 = [| 0; 4; 1; 5; 2; 3 |]

    testList
        "多最优解的标准问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Primal.create p None
              let result = solve sim
              Expect.hasLength result 2 "最优解数量错误。"
              let a, l = result[1]
              validate a l pe1 le1
              let a, l = result[0]
              validate a l pe2 le2
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p None
              let result = asolve sim
              Expect.hasLength result 2 "最优解数量错误。"
              let a, l = result[1]
              validate a l pe1 le1
              let a, l = result[0]
              validate a l pe2 le2 ]
    |> testLabel "Linear.Primal.Optimal"

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
              let sim = Primal.create p None
              let f = fun _ -> solve sim |> ignore
              Expect.throwsT<UnboundedError> f "异常类型错误。"
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p None
              let f = fun _ -> asolve sim |> ignore
              Expect.throwsT<UnboundedError> f "异常类型错误。" ]
    |> testLabel "Linear.Primal.Optimal"

[<Tests>]
let test4 =
    let p: float[,] =
        array2D
            [ [ 0; 0; 0; 0; 0.4; 0.5; 0 ]
              [ 2.7; 1; 0; 0; 0.3; 0.1; 0 ]
              [ 6; 0; 1; 0; 0.5; 0.5; 0 ]
              [ 6; 0; 0; 1; 0.6; 0.4; -1 ] ]

    let pe: float[,] =
        array2D
            [ [ -5.25; 0.5; 1; 1; 0; 0; 0 ]
              [ 0.3; 1; 0.6; -1; 0; 0; 1 ]
              [ 4.5; -5; 3; 0; 0; 1; 0 ]
              [ 7.5; 5; -1; 0; 1; 0; 0 ] ]

    let le = [| 0; 6; 5; 4; 3; 2; 1 |]

    testList
        "一般的人工问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Primal.create p <| Some 2
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p <| Some 2
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le ]
    |> testLabel "Linear.Primal.Optimal"

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

    let pe: float[,] =
        array2D
            [ [ 22; 0; 0; 1; 1; 0; 0; 0; 0 ]
              [ 3; 1; 0; -0.5; 0; 0.5; 0; 0; 0 ]
              [ 7; 0; 1; 2; -1; -2; 0; 0; 0 ]
              [ 1; 0; 0; 0.5; 0; -0.5; 1; 0; 0 ]
              [ 8; 0; 0; -2; 1; 2; 0; 1; 0 ]
              [ 6; 0; 0; 1; 0; 0; 0; 0; 1 ] ]

    let le = [| 0; 1; 2; 6; 7; 8; 3; 4; 5 |]

    testList
        "带上界约束的人工问题"
        [ testCase "同步算法"
          <| fun _ ->
              let sim = Primal.create p <| Some 4
              let result = solve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p <| Some 4
              let result = asolve sim
              Expect.hasLength result 1 "最优解数量错误。"
              let a, l = result[0]
              validate a l pe le ]
    |> testLabel "Linear.Primal.Optimal"

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
              let sim = Primal.create p <| Some 2
              let f = fun _ -> solve sim |> ignore
              Expect.throwsT<NoSolutionError> f "异常类型错误。"
          testCase "异步算法"
          <| fun _ ->
              let sim = Primal.create p <| Some 2
              let f = fun _ -> asolve sim |> ignore
              Expect.throwsT<NoSolutionError> f "异常类型错误。" ]
    |> testLabel "Linear.Primal.Optimal"
