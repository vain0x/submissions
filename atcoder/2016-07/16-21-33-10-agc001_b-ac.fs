module Program =
  open System

  [<EntryPoint>]
  let main argv = 
    let [| n; x |] = Console.ReadLine().Split([|' '|]) |> Array.map int64

    let rec calc x y =
      if y = 0L then
        0L
      else
        let l1 = (x / y) * y * 3L
        let l2 = calc y (x % y)
        in l1 + l2

    printfn "%d" (calc (n - x) x)

    // exit code
    0
