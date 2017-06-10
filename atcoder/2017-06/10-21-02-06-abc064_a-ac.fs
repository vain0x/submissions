namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|r; g; b|] = read int
    let calc () =
      let n = r * 100 + g * 10 + b
      n % 4 = 0
    printfn "%s" (if calc () then "YES" else "NO")
    0
