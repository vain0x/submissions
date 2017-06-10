namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n|] = read int
    let xs = read int
    let calc () =
      let xs = xs |> Array.sort
      (xs |> Array.last) - xs.[0]
    printfn "%d" (calc ())
    0
