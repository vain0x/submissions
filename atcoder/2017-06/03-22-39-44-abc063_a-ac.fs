namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|a; b|] = read int
    let s = a + b
    printfn "%s" (if s >= 10 then "error" else string s)
    0
