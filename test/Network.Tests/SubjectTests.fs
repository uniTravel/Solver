module Solver.Network.Tests.Subject

open System
open Expecto
open Solver.Network


let validate n adj inv cost cap msg =
    let f = fun _ -> Subject.init n adj inv cost cap |> ignore
    Expect.throwsT<ArgumentException> f "异常类型错误"
    Expect.throwsC f <| fun ex -> Expect.equal ex.Message msg "异常消息错误"

[<Tests>]
let test =
    testList
        "构造网络规划问题"
        [ testCase "根节点供应量非零"
          <| fun _ ->
              let n = [| 1; 1; 0; -1; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict []
              let cap = dict []
              let msg = $"Supply of node 0 must be zero. (Parameter 'n')"
              validate n adj inv cost cap msg
          testCase "节点供应量总和非零"
          <| fun _ ->
              let n = [| 0; 2; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict []
              let cap = dict []
              let msg = $"Summary of supply must be zero. (Parameter 'n')"
              validate n adj inv cost cap msg
          testCase "根节点有弧相连"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 0), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"Root must have no connected arc. (Parameter 'cost')"
              validate n adj inv cost cap msg
          testCase "邻接表出现非法的弧"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 5 |] |]
              let inv = [| [||]; [||] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"Graph doesn't contain arc (1,5). (Parameter 'adj')"
              validate n adj inv cost cap msg
          testCase "逆邻接表出现非法的弧"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |] |]
              let inv = [| [||]; [| 4 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"Graph doesn't contain arc (4,1). (Parameter 'inv')"
              validate n adj inv cost cap msg
          testCase "邻接表的邻接关系与弧成本字典不一致"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"There has unmatched arcs. (Parameter 'adj')"
              validate n adj inv cost cap msg
          testCase "逆邻接表的邻接关系与弧成本字典不一致"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3 |] |]
              let inv = [| [||]; [||]; [||] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"There has unmatched arcs. (Parameter 'inv')"
              validate n adj inv cost cap msg
          testCase "上界字典中出现不在成本字典中的弧"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4 |]; [| 4 |]; [||] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 2; 3 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1; (2, 4), 2; (3, 4), 1 ]
              let cap = dict [ (1, 4), 9 ]
              let msg = $"There has illegal arcs. (Parameter 'cap')"
              validate n adj inv cost cap msg
          testCase "节点供应量数组长度比弧成本字典长度大2以上"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 3), 3 ]
              let cap = dict []
              let msg = $"Graph must be connected. (Parameter 'cost')"
              validate n adj inv cost cap msg
          testCase "除根节点之外的图非连通图"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3 |]; [||] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let cap = dict []
              let msg = $"Graph must be connected. (Parameter 'cost')"
              validate n adj inv cost cap msg
          testCase "对偶算法模型无上界"
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

              let cap = dict []
              let msg = $"Arcs must have upper bound. (Parameter 'sub')"
              let f = fun _ -> Subject.init n adj inv cost cap |> Dual.create |> ignore
              Expect.throwsT<ArgumentException> f "异常类型错误"
              Expect.throwsC f <| fun ex -> Expect.equal ex.Message msg "异常消息错误"
          testCase "构造问题"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4 |]; [| 4 |]; [||] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 2; 3 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1; (2, 4), 2; (3, 4), 1 ]
              let cap = dict [ (1, 3), 9 ]
              let r1, r2, r3, r4, r5 = Subject.init n adj inv cost cap |> Subject.value
              Expect.equal r1 n "返回的节点供应量数组错误。"
              Expect.equal r2 adj "返回的节点邻接列表错误。"
              Expect.equal r3 inv "返回的节点逆邻接列表错误。"
              Expect.equal r4 cost "返回的弧成本字典错误。"
              Expect.equal r5 cap "返回的弧容量上界字典错误。" ]
    |> testLabel "Network.Subject"
