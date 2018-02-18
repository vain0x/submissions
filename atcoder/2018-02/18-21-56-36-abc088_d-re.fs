namespace global
  open System
  open System.Collections
  open System.Collections.Generic

  type Color = | Black | White

  [<AutoOpen>]
  module Operators =
    let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

  module Program =
    [<EntryPoint>]
    let main _ =
      let stoa (str: string) = str.ToCharArray()
      let neigh = [|
        1, 0
        0, -1
        -1, 0
        0, 1
      |]

      match read int with
      | [|H; W|] ->
        let board =
          Array.replicate (W + 2) (Array.replicate (H + 2) Black)
        for y in 1..H do
          board.[y] <-
            Console.ReadLine()
            |> stoa
            |> Array.map (fun c -> if c = '.' then White else Black)
        let whiteCount =
          board |> Seq.collect id |> Seq.sumBy (fun t -> if t = White then 1 else 0)

        let set = HashSet<_>()
        let queue = Queue<_>()
        queue.Enqueue(((1, 1), 1))
        let rec go () =
          if queue.Count > 0 then
            let ((y, x), d) = queue.Dequeue()
            if (y, x) = (H, W) then
              whiteCount - d
            else
              for (dy, dx) in neigh do
                let y' = y + dy
                let x' = x + dx
                if set.Add((y', x')) then
                  queue.Enqueue(((y', x'), d + 1))
              go ()
          else
            -1
        let dist = go ()
        printfn "%d" dist
      | _ -> ()
      0
