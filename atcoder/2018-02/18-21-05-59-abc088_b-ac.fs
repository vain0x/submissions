namespace global
  open System
  open System.Collections
  open System.Collections.Generic

  [<AutoOpen>]
  module Operators =
    let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

  module Program =
    [<EntryPoint>]
    let main _ =
      match (read int, read int) with
      | [|N|], A ->
        let A = A |> Array.sort |> Array.rev
        let alice = Array.sumBy snd (A |> Array.indexed |> Array.filter (fun (i, _) -> i % 2 = 0))
        let bob = Array.sumBy snd (A |> Array.indexed |> Array.filter (fun (i, _) -> i % 2 = 1))
        printfn "%d" (alice - bob)
      | _ -> ()
      0
