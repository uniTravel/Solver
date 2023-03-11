module Solver.Network.Tests.Primal

open System.Collections.Generic
open Expecto
open Solver.Network


[<Tests>]
let test =
    ptestList
        "原算法求解网络规划问题"
        [ testCase "无上界"
          <| fun _ ->
              let n = [| 0; 1; 2; -2; 0; -1 |]
              let adjacent = [ []; [ 2; 3 ]; [ 1; 3; 4; 5 ]; [ 1; 2; 4 ]; [ 2; 3; 5 ]; [ 2; 4 ] ]

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
            //   let sub = Subject.init n adjacent cost capacity
              Expect.isTrue true ""
          testCase "同步算法"
          <| fun _ ->
              let q = [| 1; 2; 3; 4 |]
              let x = HashSet<int>()
              x.Add 1 |> ignore
              x.Add 2 |> ignore
              x.Add 1 |> ignore
              Expect.isTrue true "" ]
    |> testLabel "Network.Primal"
