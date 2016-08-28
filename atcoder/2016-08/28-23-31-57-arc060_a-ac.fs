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
    let [|n; a|] = readLine int
    let xs = readLine int

    let maxX = Seq.concat [[|a|]; xs] |> Seq.max
    let maxSum = n * maxX + 1

    // dp.[j].[k].[s] = c:
    //   xs.[0..(j - 1)] から k 個を選び、その総和が s に等しいような選び方の総数が c
    let dp =
      Array.init (n + 1) (fun _ ->
        Array.init (n + 1) (fun _ ->
          Array.zeroCreate (maxSum + 1)
          ))
    dp.[0].[0].[0] <- 1L
    for j in 1..n do
      for k in 0..n do
        for s in 0..maxSum do
          dp.[j].[k].[s] <-
            if s < xs.[j - 1] then
              dp.[j - 1].[k].[s]
            elif k >= 1 && s >= xs.[j - 1] then
              dp.[j - 1].[k].[s] + dp.[j - 1].[k - 1].[s - xs.[j - 1]]
            else 0L

    let count =
      (seq { 0..n } |> Seq.sumBy (fun k -> dp.[n].[k].[k * a])) - 1L

    printfn "%d" count

    // exit code
    0
