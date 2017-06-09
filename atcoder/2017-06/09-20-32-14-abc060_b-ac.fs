namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|a; b; c|] = read int

    let rec gcd m n =
      if m < n then
        gcd n m
      else if n = 0 then
        m
      else
        gcd (m % n) n

    let calc () =
      // a * x + b * y = c となる (x, y) がある
      // ⇔ gcd(a, b) | c
      let g = gcd a b
      c % g = 0

    printfn "%s" (if calc () then "YES" else "NO")

    0
