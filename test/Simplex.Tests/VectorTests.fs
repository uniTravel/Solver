module Simplex.Tests.Vector

open System.Numerics
open Expecto


[<Tests>]
let test =
    testList "向量点乘" [
        testCase "都是单值构造的向量" <| fun _ ->
              let result = Vector.Dot(Vector 1.0, Vector 2.0)
              Expect.equal result 8.0 ""
        testCase "单值与短数组构造的向量" <| fun _ ->
              let f = fun _ -> Vector.Dot(Vector 1.0, Vector [| for i in 1 .. 2 -> 2.0 |]) |> ignore
              Expect.throwsC f (fun ex -> printfn $"%s{ex.Message}")
        testCase "单值与长数组构造的向量" <| fun _ ->
            let v1 = Vector 1.0
            let v2 = Vector [| for i in 1 .. 6 -> 2.0 |]
            let result = Vector.Dot(v1, v2)
            Expect.equal result 8.0 ""
        testCase "单值与标准数组构造的向量" <| fun _ ->
            let v1 = Vector 1.0
            let v2 = Vector [| for i in 1 .. 4 -> 2.0 |]
            let result = Vector.Dot(v1, v2)
            Expect.equal result 8.0 ""
        testCase "两个短数组构造的向量" <| fun _ ->
            let f = fun _ -> Vector.Dot(Vector [| for i in 1 .. 2 -> 1.0 |], Vector [| for i in 1 .. 2 -> 2.0 |]) |> ignore
            Expect.throwsC f (fun ex -> printfn $"%s{ex.Message}")
        testCase "两个长数组构造的向量" <| fun _ ->
            let v1 = Vector [| for i in 1 .. 6 -> 1.0 |]
            let v2 = Vector [| for i in 1 .. 6 -> 2.0 |]
            let result = Vector.Dot(v1, v2)
            Expect.equal result 8.0 ""
        testCase "两个标准数组构造的向量" <| fun _ ->
            let v1 = Vector [| for i in 1 .. 4 -> 1.0 |]
            let v2 = Vector [| for i in 1 .. 4 -> 2.0 |]
            let result = Vector.Dot(v1, v2)
            Expect.equal result 8.0 ""
    ] |> testLabel "Simplex"