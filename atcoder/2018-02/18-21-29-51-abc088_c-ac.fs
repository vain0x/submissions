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
      match [|for _ in 0..2 -> read int|] with
      | [|
          [|c11; c12; c13|]
          [|c21; c22; c23|]
          [|c31; c32; c33|]
        |] ->
        let db1 = c12 - c11
        let db2 = c13 - c12
        let da1 = c21 - c11
        let da2 = c31 - c21
        let ok =
          c22 - c12 = da1 && c23 - c13 = da1
          && c32 - c22 = da2 && c33 - c23 = da2
          && c22 - c21 = db1 && c32 - c31 = db1
          && c23 - c22 = db2 && c33 - c32 = db2
        printfn "%s" (if ok then "Yes" else "No")
      | _ -> ()
      0
