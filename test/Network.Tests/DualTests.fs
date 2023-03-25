module Solver.Network.Tests.Dual

open Expecto
open Solver.Network


[<Tests>]
let test =
    testList
        "对偶算法求解网络规划问题"
        [ testCase ""
          <| fun _ ->
              let q = 1
              Expect.isTrue true "" ]
    |> testLabel "Network.Dual"
