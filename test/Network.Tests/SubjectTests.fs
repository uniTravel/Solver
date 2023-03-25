module Solver.Network.Tests.Subject

open System
open Expecto
open Solver.Network


let validate n adj inv cost capacity msg =
    let f = fun _ -> Subject.init n adj inv cost capacity |> ignore
    Expect.throwsT<ArgumentException> f "异常类型错误。"
    Expect.throwsC f <| fun ex -> Expect.equal ex.Message msg "异常消息错误。"

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
              let capacity = dict []
              let msg = $"Supply of node 0 must be zero. (Parameter 'n')"
              validate n adj inv cost capacity msg
          testCase "节点供应量总和非零"
          <| fun _ ->
              let n = [| 0; 2; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict []
              let capacity = dict []
              let msg = $"Summary of supply must be zero. (Parameter 'n')"
              validate n adj inv cost capacity msg
          testCase "根节点有弧相连"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 0), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"Root must have no connected arc. (Parameter 'cost')"
              validate n adj inv cost capacity msg
          testCase "邻接表出现非法的弧"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 5 |] |]
              let inv = [| [||]; [||] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"Graph doesn't contain arc (1,5). (Parameter 'adj')"
              validate n adj inv cost capacity msg
          testCase "逆邻接表出现非法的弧"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |] |]
              let inv = [| [||]; [| 4 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"Graph doesn't contain arc (4,1). (Parameter 'inv')"
              validate n adj inv cost capacity msg
          testCase "邻接表的邻接关系与弧成本字典不一致"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"There has unmatched arcs. (Parameter 'adj')"
              validate n adj inv cost capacity msg
          testCase "逆邻接表的邻接关系与弧成本字典不一致"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3 |] |]
              let inv = [| [||]; [||]; [||] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"There has unmatched arcs. (Parameter 'inv')"
              validate n adj inv cost capacity msg
          testCase "节点供应量数组长度比弧成本字典长度大2以上"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [||]
              let inv = [||]
              let cost = dict [ (1, 2), 2; (1, 3), 3 ]
              let capacity = dict []
              let msg = $"Graph must be connected. (Parameter 'cost')"
              validate n adj inv cost capacity msg
          testCase "除根节点之外的图非连通图"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3 |]; [||] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1 ]
              let capacity = dict []
              let msg = $"Graph must be connected. (Parameter 'cost')"
              validate n adj inv cost capacity msg
          testCase "构造问题"
          <| fun _ ->
              let n = [| 0; 1; 0; 0; -1 |]
              let adj = [| [||]; [| 2; 3 |]; [| 3; 4 |]; [| 4 |]; [||] |]
              let inv = [| [||]; [||]; [| 1 |]; [| 1; 2 |]; [| 2; 3 |] |]
              let cost = dict [ (1, 2), 2; (1, 3), 3; (2, 3), 1; (2, 4), 2; (3, 4), 1 ]
              let capacity = dict [ (1, 3), 9 ]
              let r1, r2, r3, r4, r5 = Subject.init n adj inv cost capacity |> Subject.value
              Expect.equal r1 n "返回的节点供应量数组错误。"
              Expect.equal r2 adj "返回的节点邻接列表错误。"
              Expect.equal r3 inv "返回的节点逆邻接列表错误。"
              Expect.equal r4 cost "返回的弧成本字典错误。"
              Expect.equal r5 capacity "返回的弧容量上界字典错误。" ]
    |> testLabel "Network.Subject"
