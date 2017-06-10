namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  type Color =
    | Gray
    | Brown
    | Green
    | LightBlue
    | Blue
    | Yellow
    | Orange
    | Red

  let colorFromRate rate =
    if 1 <= rate && rate <= 399 then
      Some Gray
    else if 400 <= rate && rate <= 799 then
      Some Brown
    else if 800 <= rate && rate <= 1199 then
      Some Green
    else if 1200 <= rate && rate <= 1599 then
      Some LightBlue
    else if 1600 <= rate && rate <= 1999 then
      Some Blue
    else if 2000 <= rate && rate <= 2399 then
      Some Yellow
    else if 2400 <= rate && rate <= 2799 then
      Some Orange
    else if 2800 <= rate && rate <= 3199 then
      Some Red
    else
      None

  [<EntryPoint>]
  let main _ =
    let [|n|] = read int
    let rs = read int
    let calc () =
      let colors = rs |> Array.map colorFromRate
      let distinctColors = colors |> Array.choose id |> Array.distinct
      let anyColorPlayerCount = colors |> Seq.filter ((=) None) |> Seq.length
      let minColorCount = distinctColors.Length |> max 1
      let maxColorCount = distinctColors.Length + anyColorPlayerCount
      (minColorCount, maxColorCount)

    let (min, max) = calc ()
    printfn "%d %d" min max
    0
