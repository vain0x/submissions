namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let n = Console.ReadLine() |> int
    let scores =
      [|
        for _ in 0..(n - 1) ->
          Console.ReadLine() |> int
      |]
    let maxScore = scores |> Array.sum

    // dp.[i].[s]:
    // 最初の (i + 1) 個の配点を望む組み合わせで加算して、
    // 和をちょうど s にできるとき真。
    let dp =
      [|0..(n - 1)|] |> Array.map (fun _ -> Array.zeroCreate (maxScore + 1))
      
    for i in 0..(n - 1) do
      for s in 0..maxScore do
        let t = scores.[i]
        dp.[i].[s] <-
          if i = 0 then
            s = 0 || s = t
          else
            dp.[i - 1].[s] || (s >= t && dp.[i - 1].[s - t])

    let result =
      [|
        for s in 0..maxScore do
          if dp.[n - 1].[s] then
            yield if s % 10 = 0 then 0 else s
      |]
      |> Array.max

    printfn "%d" result

    0
