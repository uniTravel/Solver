module Solver.Network.Tests.Primal

open System.Collections.Generic
open Expecto
open Solver.Network


[<PTests>]
let test =
    testList
        "原算法求解网络规划问题"
        [ testCase ""
          <| fun _ ->
              let n = [||]
              let adjacent = []
              let cost = dict []
              let capacity = dict []
              let sub = Subject.init n adjacent cost capacity
              Expect.isTrue true ""
          ftestCase "同步算法"
          <| fun _ ->
              let q = [| 1; 2; 3; 4 |]
              let x = HashSet<int>()
              x.Add 1 |> ignore
              x.Add 2 |> ignore
              x.Add 1 |> ignore
              Expect.isTrue true "" ]
    |> testLabel "Network.Primal"
