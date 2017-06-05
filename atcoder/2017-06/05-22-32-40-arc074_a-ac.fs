namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|h; w|] = read int64

    let calc () =
      let crossCase h w =
        seq {
          for y in 1L..(h - 1L) do
            let x = w / 2L
            let s1 = y * (w - x)
            let s2 = y * x
            let s3 = (h - y) * x
            let s4 = (h - y) * (w - x)
            yield (s1 + s2, s3, s4)
            yield (s2 + s3, s4, s1)
            yield (s3 + s4, s1, s2)
            yield (s4 + s1, s2, s3)
        }
      let noncrossCase h w =
        seq {
          for y1 in 1L..(h - 2L) do
            let y2 = y1 + (h - y1) / 2L
            yield (y1 * w, (y2 - y1) * w, (h - y2) * w)
        }
      let cases =
        crossCase h w
        |> Seq.append (crossCase w h)
        |> Seq.append (noncrossCase h w)
        |> Seq.append (noncrossCase w h)
      seq {
        for (s0, s1, s2) in cases ->
          let max = s0 |> max s1 |> max s2
          let min = s0 |> min s1 |> min s2
          max - min
      } |> Seq.min

    printfn "%d" (calc ())

    0
