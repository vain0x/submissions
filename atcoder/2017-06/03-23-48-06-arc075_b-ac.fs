namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n; a; b|] = read int64
    let n = int n
    let hs =
      [|
        for _ in 0..(n - 1) ->
          Console.ReadLine() |> int64
      |]

    let calc () =
      let isOk k =
        // i 番目の敵を中心に xs.[i] 回攻撃するとする。
        // 敵を全滅させなければいけないので、すべての敵 i について、
        // hs.[i] <= (敵 i に与えるダメージ) = (a - b) * xs.[i] + b * k
        // が成り立つ必要がある。これが各 xs.[i] の下限を定める。
        // また、k 回の攻撃で全滅させなければいけないので、xs の総和は k 以下でなければいけない。
        let xs =
          [|
            for i in 0..(n - 1) ->
              (hs.[i] - b * k + (a - b - 1L)) / (a - b) |> max 0L
          |]
        Array.sum xs <= k

      // [めぐる式2分探索](https://twitter.com/meguru_comp/status/697008509376835584)
      let rec loop ok ng =
        if abs (ok - ng) <= 1L then
          ok
        else
          let k = (ok + ng) / 2L
          if isOk k then
            loop k ng
          else
            loop ok k
      loop (Array.max hs) 0L

    printfn "%d" (calc ())

    0
