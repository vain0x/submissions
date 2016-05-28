namespace AdhocFs

open System

module Program =
  [<EntryPoint>]
  let main argv = 
    let s1 = Console.ReadLine().Split(' ') |> Array.map int
    let s2 = Console.ReadLine().Split(' ') |> Array.map int

    let b =
      [ for l1 in s1 do
        for l2 in s2 -> l1 = l2
      ]
      |> List.exists id

    printfn "%s" (if b then "YES" else "NO")

    // exit code
    0
