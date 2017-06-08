namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  let bellmanFord (g: array<array<_>>) v =
    let inf = Int64.MaxValue
    let dist = [|for _ in 0..(g.Length - 1) -> inf|]
    dist.[v] <- 0L
    let rec loop1 i =
      if i < g.Length then
        let rec loop2 u =
          if u < g.Length then
            let rec loop3 e =
              if e < g.[u].Length then
                let (v, w) = g.[u].[e]
                if dist.[u] <> inf && dist.[v] > dist.[u] + w then
                  dist.[v] <- dist.[u] + w
                  if i = g.Length - 1 then
                    None
                  else
                    loop3 (e + 1)
                else
                  loop3 (e + 1)
              else
                loop2 (u + 1)
            loop3 0
          else
            loop1 (i + 1)
        loop2 0
      else
        Some dist
    loop1 0

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
      // スコアを -1 倍することで、最大化問題を最小化問題に変換する。
      let g =
        let g = [|for _ in 0..(n - 1) -> ResizeArray()|]
        for (u, v, w) in edges do
          g.[u].Add((v, -w))
        g |> Array.map (fun list -> list.ToArray())
      bellmanFord g 0
      |> Option.map (fun dist -> (- dist.[n - 1]))

    match calc () with
    | Some score ->
      printfn "%d" score
    | None ->
      printfn "inf"

    0
