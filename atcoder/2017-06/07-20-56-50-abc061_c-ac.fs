namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n; k|] = read int64
    let n = int n

    let ps =
      [|
        for i in 0..(n - 1) do
          let [|a; b|] = read int64
          yield (a, b)
      |]

    let calc () =
      let ps = ps |> Seq.sortBy fst |> Seq.toArray
      let rec loop k i =
        if i < ps.Length then
          let (a, b) = ps.[i]
          if 1L <= k && k <= b then
            a
          else
            loop (k - b) (i + 1)
        else
          failwith "N/A"
      loop k 0

    printfn "%d" (calc ())

    0
