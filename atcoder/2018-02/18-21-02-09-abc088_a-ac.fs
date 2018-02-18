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
      | [|N|], [|A|] ->
        printfn "%s" (if (N % 500) <= A then "Yes" else "No")
      | _ -> ()
      0
