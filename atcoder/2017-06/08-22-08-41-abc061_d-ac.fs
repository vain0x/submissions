namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic
open System.Linq

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  let inf = Int64.MaxValue / 3L

  // Bellman-Ford アルゴリズムの改良版
  // 点 source から点 sink への最短経路を探索し、経路の重みの最小値を求める。
  // 負閉路を経由して sink に到達できる場合、最小値はないので、 None を返す。
  let bellmanFord source sink (g: array<array<_>>) =
    let n = g.Length

    // dist.[u] = w
    // ⇔ 点 source から u への経路の重みの最小値が w である。
    let dist =
      let dist = [|for _ in 0..(n - 1) -> inf|]
      dist.[source] <- 0L
      for _ in 0..(n - 2) do
        for u in 0..(n - 1) do
          for (v, w) in g.[u] do
            if dist.[u] < inf && dist.[v] > dist.[u] + w then
              dist.[v] <- dist.[u] + w
      dist

    // unbounded.[u] = true
    // ⇔ 負閉路を経由して点 u に到達できる。
    let unbounded =
      let unbounded = [|for _ in 0..(n - 1) -> false|]
      for _ in 0..(n - 1) do
        for u in 0..(n - 1) do
          for (v, w) in g.[u] do
            // 負閉路を経由して u に到達できるなら、v にも到達できる。
            // v の最小値が dist.[v] より小さくなるなら、無限に小さくなるので、v は負閉路に含まれている。
            if unbounded.[u] || (dist.[u] < inf && dist.[v] > dist.[u] + w) then
              unbounded.[v] <- true
      unbounded

    if unbounded.[sink] then None else Some dist.[sink]

  let adjacencyList n (edges: array<_>) =
    [|
      let edgesFromSource = edges.ToLookup(fun (u, _, _) -> u)
      for u in 0..(n - 1) ->
        [|
          for (_, v, w) in edgesFromSource.[u] -> (v, w)
        |]
    |]

  [<EntryPoint>]
  let main _ =
    let [|n; m|] = read int
    let edges =
      [|
        for _ in 0..(m - 1) do
          let [|a; b; c|] = read int64
          yield (int a - 1, int b - 1, c)
      |]

    let calc () =
      // スコアを -1 倍することで、最大化問題を最小化問題 (最短経路問題) に変換する。
      let edges = [|for (u, v, w) in edges -> (u, v, -w)|]
      adjacencyList n edges
      |> bellmanFord 0 (n - 1)
      |> Option.map (~-)

    match calc () with
    | Some score ->
      printfn "%d" score
    | None ->
      printfn "inf"

    0
