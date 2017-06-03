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
        for i in 0..(n - 1) ->
          Console.ReadLine() |> int
      |]

    let calc () =
      let total = scores |> Array.sum
      if total % 10 = 0 then
        let ts = scores |> Array.filter (fun x -> x % 10 <> 0)
        if ts.Length = 0 then
          0
        else
          let min = ts |> Array.min
          total - min
      else
        total

    printfn "%d" (calc ())
    0
