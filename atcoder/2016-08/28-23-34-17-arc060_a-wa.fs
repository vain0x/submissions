namespace DotNetLab.Fs.Console

open System
open System.Collections.Generic

[<AutoOpen>]
module Misc =
  type Long = int64
  let inline long x = int64 x

  let readLine f = Console.ReadLine().Split([|' '|]) |> Array.map f

  let combinationSums xs =
    let rec loop (s, n) i =
      seq {
        if i = (xs |> Array.length) then
          yield (s, n)
        else
          yield! loop (s, n) (i + 1)
          yield! loop (s + xs.[i], n + 1.0) (i + 1)
      }
    in
      loop (0.0, 0.0) 0

  let halves xs =
    xs
    |> combinationSums
    |> Seq.toArray
    |> Array.sortBy (fun (s, n) -> s / n)

  let upperBound pred (xs: array<'x>) =
    let rec loop lb ub =
      if ub - lb <= 1
      then ub
      else
        let i = (ub - lb) / 2 + lb
        if pred xs.[i]
        then loop i ub
        else loop lb i
    in
      loop (-1) (xs |> Array.length)

  let countHalf a (s, n) (hs: (float * float)[]) =
    let lb = hs |> upperBound (fun (s', n') -> (s + s') / (n + n') < a)
    let ub = hs |> upperBound (fun (s', n') -> (s + s') / (n + n') <= a)
    ub - lb |> long

module Program =
  [<EntryPoint>]
  let main _ =
    let [|_; a|] = readLine float
    let xs = readLine float

    let n = xs.Length / 2
    let ys = xs |> Array.skip n
    let xs = xs |> Array.take n

    let xhs = xs |> halves
    let yhs = ys |> halves

    let count = xhs |> Seq.sumBy (fun h -> countHalf a h yhs)

    printfn "%d" count

    // exit code
    0
