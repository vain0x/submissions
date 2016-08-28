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

  let count pred xs =
    let mutable k = 0L
    for x in xs do
      if pred x then k <- k + 1L
    k

module Program =
  [<EntryPoint>]
  let main _ =
    let [|_; a|] = readLine float
    let xs = readLine float

    let count =
      xs |> combinationSums |> count (fun (s, n) -> (s / n) = a)

    printfn "%d" count

    // exit code
    0
