module Solver.Network.Tests.Primal

open System.Collections.Generic
open Expecto


[<Tests>]
let test =
    testList
        "一般的标准问题"
        [ testCase "同步算法"
          <| fun _ ->
              let l1 = [ (1, 2), 2; (2, 3), 3; (3, 4), 3 ]
              let l2 = [ (1, 3), 1; (2, 4), 3 ]
              let a = l1 |> List.maxBy snd
              let b = l2 |> List.maxBy snd
              let c = l1 @ l2 |> List.maxBy snd
              let d = l2 @ l1 |> List.maxBy snd
              let f = List.rev l1 @ l2
              let e = List.rev l1 @ l2 |> List.maxBy snd
              let g = ((0, 0), 0) :: l1
              let h = g[1]
              let q = [| 1; 2; 3; 4 |]
              let x = q[1..]
              Expect.isTrue true "" ]
    |> testLabel "Network.Primal"
