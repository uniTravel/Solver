module Solver.Network.Tests.Dual

open Expecto
open Solver.Network


[<Tests>]
let test1 =
    testList
        "对偶Primal-Dual方法"
        [ testCase "有上界，无负成本"
          <| fun _ ->
              let n = [| 0; 50; 40; 0; -30; -60 |]
              let adj = [| [||]; [| 2; 3; 4 |]; [| 3 |]; [| 5 |]; [| 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 1; 5 |]; [| 3; 4 |] |]

              let cost =
                  dict [ (1, 2), 2; (1, 3), 4; (2, 3), 3; (1, 4), 9; (3, 5), 1; (4, 5), 3; (5, 4), 2 ]

              let cap =
                  dict
                      [ (1, 2), 10
                        (1, 3), 120
                        (2, 3), 120
                        (1, 4), 120
                        (3, 5), 80
                        (4, 5), 120
                        (5, 4), 120 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 40
                        (1, 4), 10
                        (2, 3), 40
                        (4, 5), 0
                        (5, 4), 20
                        (3, 5), 80 ]

              match Dual.create sub |> Dual.pd with
              | Optimal x ->
                  Expect.hasLength x 7 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解"
          testCase "有上界，有负成本"
          <| fun _ ->
              let n = [| 0; 1; 2; -2; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4; 5 |]; [| 2 |]; [| 3; 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1; 3 |]; [| 1; 2; 4 |]; [| 2; 5 |]; [| 2; 4 |] |]

              let cost =
                  dict
                      [ (1, 2), 5
                        (1, 3), 2
                        (2, 3), 6
                        (3, 2), 3
                        (2, 4), -2
                        (4, 3), 2
                        (2, 5), 2
                        (4, 5), 3
                        (5, 4), 0 ]

              let cap =
                  dict
                      [ (1, 2), 2
                        (1, 3), 1
                        (2, 3), 2
                        (3, 2), 1
                        (2, 4), 1
                        (4, 3), 3
                        (2, 5), 10
                        (4, 5), 10
                        (5, 4), 5 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 1
                        (2, 3), 0
                        (3, 2), 0
                        (2, 4), 1
                        (4, 3), 1
                        (2, 5), 1
                        (4, 5), 0
                        (5, 4), 0 ]

              match Dual.create sub |> Dual.pd with
              | Optimal x ->
                  Expect.hasLength x 9 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解" ]
    |> testLabel "Network.Dual"

[<Tests>]
let test2 =
    testList
        "对偶Sequencial Shortest Path方法"
        [ testCase "有上界，无负成本"
          <| fun _ ->
              let n = [| 0; 50; 40; 0; -30; -60 |]
              let adj = [| [||]; [| 2; 3; 4 |]; [| 3 |]; [| 5 |]; [| 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 1; 5 |]; [| 3; 4 |] |]

              let cost =
                  dict [ (1, 2), 2; (1, 3), 4; (2, 3), 3; (1, 4), 9; (3, 5), 1; (4, 5), 3; (5, 4), 2 ]

              let cap =
                  dict
                      [ (1, 2), 10
                        (1, 3), 120
                        (2, 3), 120
                        (1, 4), 120
                        (3, 5), 80
                        (4, 5), 120
                        (5, 4), 120 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 40
                        (1, 4), 10
                        (2, 3), 40
                        (4, 5), 0
                        (5, 4), 20
                        (3, 5), 80 ]

              match Dual.create sub |> Dual.ssp with
              | Optimal x ->
                  Expect.hasLength x 7 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解"
          testCase "有上界，有负成本"
          <| fun _ ->
              let n = [| 0; 1; 2; -2; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4; 5 |]; [| 2 |]; [| 3; 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1; 3 |]; [| 1; 2; 4 |]; [| 2; 5 |]; [| 2; 4 |] |]

              let cost =
                  dict
                      [ (1, 2), 5
                        (1, 3), 2
                        (2, 3), 6
                        (3, 2), 3
                        (2, 4), -2
                        (4, 3), 2
                        (2, 5), 2
                        (4, 5), 3
                        (5, 4), 0 ]

              let cap =
                  dict
                      [ (1, 2), 2
                        (1, 3), 1
                        (2, 3), 2
                        (3, 2), 1
                        (2, 4), 1
                        (4, 3), 3
                        (2, 5), 10
                        (4, 5), 10
                        (5, 4), 5 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 1
                        (2, 3), 0
                        (3, 2), 0
                        (2, 4), 1
                        (4, 3), 1
                        (2, 5), 1
                        (4, 5), 0
                        (5, 4), 0 ]

              match Dual.create sub |> Dual.ssp with
              | Optimal x ->
                  Expect.hasLength x 9 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解" ]
    |> testLabel "Network.Dual"

[<Tests>]
let test3 =
    ptestList
        "对偶Relaxation方法"
        [ testCase "有上界，无负成本"
          <| fun _ ->
              let n = [| 0; 50; 40; 0; -30; -60 |]
              let adj = [| [||]; [| 2; 3; 4 |]; [| 3 |]; [| 5 |]; [| 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 1; 5 |]; [| 3; 4 |] |]

              let cost =
                  dict [ (1, 2), 2; (1, 3), 4; (2, 3), 3; (1, 4), 9; (3, 5), 1; (4, 5), 3; (5, 4), 2 ]

              let cap =
                  dict
                      [ (1, 2), 10
                        (1, 3), 120
                        (2, 3), 120
                        (1, 4), 120
                        (3, 5), 80
                        (4, 5), 120
                        (5, 4), 120 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 40
                        (1, 4), 10
                        (2, 3), 40
                        (4, 5), 0
                        (5, 4), 20
                        (3, 5), 80 ]

              match Dual.create sub |> Dual.rex with
              | Optimal x ->
                  Expect.hasLength x 7 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解"
          testCase "有上界，有负成本"
          <| fun _ ->
              let n = [| 0; 1; 2; -2; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4; 5 |]; [| 2 |]; [| 3; 5 |]; [| 4 |] |]
              let inv = [| [||]; [||]; [| 1; 3 |]; [| 1; 2; 4 |]; [| 2; 5 |]; [| 2; 4 |] |]

              let cost =
                  dict
                      [ (1, 2), 5
                        (1, 3), 2
                        (2, 3), 6
                        (3, 2), 3
                        (2, 4), -2
                        (4, 3), 2
                        (2, 5), 2
                        (4, 5), 3
                        (5, 4), 0 ]

              let cap =
                  dict
                      [ (1, 2), 2
                        (1, 3), 1
                        (2, 3), 2
                        (3, 2), 1
                        (2, 4), 1
                        (4, 3), 3
                        (2, 5), 10
                        (4, 5), 10
                        (5, 4), 5 ]

              let sub = Subject.init n adj inv cost cap

              let sx =
                  dict
                      [ (1, 2), 0
                        (1, 3), 1
                        (2, 3), 0
                        (3, 2), 0
                        (2, 4), 1
                        (4, 3), 1
                        (2, 5), 1
                        (4, 5), 0
                        (5, 4), 0 ]

              match Dual.create sub |> Dual.rex with
              | Optimal x ->
                  Expect.hasLength x 9 "解集大小错误"
                  Expect.containsAll x sx "解集不一致"
              | _ -> failtest "应该返回最优解" ]
    |> testLabel "Network.Dual"
