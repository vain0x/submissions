namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  let bellmanFord (g: array<array<_>>) v =
    let inf = Int64.MaxValue / 3L
    let n = g.Length
    let dist = [|for _ in 0..(n - 1) -> inf|]
    dist.[v] <- 0L
    let tryUpdate () =
      let mutable didUpdate = false
      for u in 0..(n - 1) do
        for (v, w) in g.[u] do
          if dist.[u] < inf then
            let d = dist.[u] + w
            if dist.[v] > d then
              dist.[v] <- d
              didUpdate <- true
      didUpdate
    for i in 0..(n - 2) do
      tryUpdate () |> ignore
    if tryUpdate () then
      None
    else
      Some dist

  let adjacencyList n edges =
    let g = [|for _ in 0..(n - 1) -> ResizeArray()|]
    for (u, v, w) in edges do
      g.[u].Add((v, w))
    g |> Array.map (fun list -> list.ToArray())

  let reachableNodesTo sink (n, m, (edges: array<_>)) =
    // 双対グラフの隣接リスト表現
    // 辺の重みは、その辺の番号を表す。
    let g = [|for _ in 0..(n - 1) -> ResizeArray()|]
    for j in 0..(m - 1) do
      let (u, v, _) = edges.[j]
      g.[v].Add((u, j))

    let reachable = [|for e in 0..(m - 1) -> false|]
    let visited = [|for u in 0..(n - 1) -> false|]
    let queue = Queue<_>()
    queue.Enqueue(sink)
    while queue.Count > 0 do
      let u = queue.Dequeue()
      if not visited.[u] then
        visited.[u] <- true
        for (v, j) in g.[u] do
          reachable.[j] <- true
          queue.Enqueue(v)

    [|
      for j in 0..(m - 1) do
        if reachable.[j] then
          yield edges.[j]
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
      // 終点に到達できない辺を除去する。
      let edges = (n, m, edges) |> reachableNodesTo (n - 1)
      // スコアを -1 倍することで、最大化問題を最小化問題 (最短経路問題) に変換する。
      let edges = [|for (u, v, w) in edges -> (u, v, -w)|]
      let g = adjacencyList n edges
      bellmanFord g 0
      |> Option.map (fun dist -> (- dist.[n - 1]))

    match calc () with
    | Some score ->
      printfn "%d" score
    | None ->
      printfn "inf"

    0
