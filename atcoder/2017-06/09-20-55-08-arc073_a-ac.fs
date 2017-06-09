namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n; t|] = read int64
    let n = int n
    let ts = read int64

    ts
    |> Array.collect (fun s -> [|(s, 1); (s + t, -1)|])
    |> Array.sortBy fst
    |> Array.pairwise
    |> Array.fold
      (fun (depth, total) ((first, _), (last, diff)) ->
        (depth + diff, (total + (if depth > 0 then last - first else 0L)))
      ) (1, 0L)
    |> (fun (_, total) -> printfn "%d" total)

    0
