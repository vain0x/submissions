module Program =
  open System

  [<EntryPoint>]
  let main argv = 
    let [| n; x |] = Console.ReadLine().Split([|' '|]) |> Array.map int

    let rec calc x y =
      let l1 = (x / y) * y * 3
      let l2 =
        let r = x % y
        if r = 0
        then 0
        else calc y r
      in l1 + l2

    printfn "%d" (calc (n - x) x)

    // exit code
    0
