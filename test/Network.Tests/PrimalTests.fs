module Solver.Network.Tests.Primal

open Expecto
open Solver.Network


[<Tests>]
let test =
    testList
        "原算法求解网络规划问题"
        [ testCase "无上界"
          <| fun _ ->
              let n = [| 0; 1; 2; -2; 0; -1 |]
              let adj = [ []; [ 2; 3 ]; [ 3; 4; 5 ]; [ 2 ]; [ 3; 5 ]; [ 4 ] ]
              let inv = [ []; []; [ 1; 3 ]; [ 1; 2; 4 ]; [ 2; 5 ]; [ 2; 4 ] ]

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

              let capacity = dict []
              let sub = Subject.init n adj inv cost capacity
              let sx = dict [ (1, 3), 1; (2, 4), 2; (4, 3), 1; (4, 5), 1 ]

              match Primal.create sub |> Primal.solve 10 with
              | Optimal(x, _) ->
                  Expect.hasLength x 5 "解集大小错误。"
                  Expect.containsAll x sx "解集不一致。"
              | _ -> failtest "应该返回最优解。"
          testCase "包含上界约束"
          <| fun _ ->
              let n = [| 0; 50; 40; 0; -30; -60 |]
              let adj = [ []; [ 2; 3; 4 ]; [ 3 ]; [ 5 ]; [ 5 ]; [ 4 ] ]
              let inv = [ []; []; [ 1 ]; [ 1; 2 ]; [ 1; 5 ]; [ 3; 4 ] ]

              let cost =
                  dict [ (1, 2), 2; (1, 3), 4; (2, 3), 3; (1, 4), 9; (3, 5), 1; (4, 5), 3; (5, 4), 2 ]

              let capacity = dict [ (1, 2), 10; (3, 5), 80 ]
              let sub = Subject.init n adj inv cost capacity
              let sx = dict [ (1, 3), 40; (1, 4), 10; (2, 3), 40; (5, 4), 20; (0, 5), 0 ]
              let su = dict [ (3, 5), 80 ]

              match Primal.create sub |> Primal.solve 10 with
              | Optimal(x, u) ->
                  Expect.hasLength x 5 "解集大小错误。"
                  Expect.hasLength u 1 "上界解集大小错误。"
                  Expect.containsAll x sx "解集不一致。"
                  Expect.containsAll u su "上界解集不一致。"
              | _ -> failtest "应该返回最优解。" ]
    |> testLabel "Network.Primal"
