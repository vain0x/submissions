namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let s = Console.ReadLine()
    (if s |> Seq.distinct |> Seq.length = s.Length then "yes" else "no") |> printfn "%s"
    0
