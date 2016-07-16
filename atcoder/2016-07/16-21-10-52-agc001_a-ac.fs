module Program =
  open System

  [<EntryPoint>]
  let main argv = 
    let n = Console.ReadLine() |> int
    let xs = Console.ReadLine().Split([|' '|]) |> Array.map int

    let m =
      xs
      |> Seq.sort
      |> Seq.pairwise
      |> Seq.indexed
      |> Seq.filter (fun (i, p) -> i % 2 = 0)
      |> Seq.map (fun (i, (l, r)) -> min l r)
      |> Seq.sum

    printfn "%d" m

    // exit code
    0
