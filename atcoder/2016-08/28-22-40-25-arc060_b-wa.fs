namespace DotNetLab.Fs.Console

open System
open System.Collections.Generic

[<AutoOpen>]
module Misc =
  type Long = int64
  let inline long x = int64 x

  let readLine f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n|] = readLine long
    let [|s|] = readLine long

    let f b =
      let rec loop n =
        if n < b
        then n
        else loop (n / b) + (n % b)
      in
        loop n

    let b =
      let rec loop lb ub =
        if ub - lb = 0L || (ub - lb = 1L && f ub <> s) then -1L
        elif ub - lb = 1L then ub
        else
          let b = (ub - lb) / 2L + lb
          if f b <= s
          then loop b ub
          else loop lb b
      in
        loop 2L Long.MaxValue

    printfn "%d" b

    // exit code
    0
