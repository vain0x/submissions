namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n; m|] = read int
    let edges =
      [|
        for _ in 0..(m - 1) do
          let [|a; b|] = read int
          yield (a - 1, b - 1)
      |]

    let calc () =
      let degrees = [|for _ in 0..(n - 1) -> 0|]
      for (a, b) in edges do
        degrees.[a] <- degrees.[a] + 1
        degrees.[b] <- degrees.[b] + 1
      degrees

    calc () |> Seq.iter (printfn "%d")
    0
