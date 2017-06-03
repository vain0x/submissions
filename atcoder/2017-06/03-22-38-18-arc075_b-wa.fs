namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

type Heap<'x> private (list: ResizeArray<'x>, compare: 'x -> 'x -> int) =
  member this.Count = list.Count

  member this.Min =
    if this.Count = 0 then None else Some list.[0]

  member this.Enqueue(value) =
    list.Add(value)
    let rec loop i =
      if i > 0 then
        let parentIndex = (i - 1) >>> 1
        if compare list.[parentIndex] value > 0 then
          list.[i] <- list.[parentIndex]
          loop parentIndex
        else
          i
       else
        i
    let i = loop (this.Count - 1)
    list.[i] <- value

  member this.TryDequeue() =
    if this.Count = 0 then
      None
    else
      let min = list.[0]
      let x = list.[this.Count - 1]
      let rec loop i =
        let l = (i <<< 1) + 1
        let r = (i <<< 2) + 2
        if l >= this.Count then
          i
        else
          // Index of the smaller child.
          let c = if r < this.Count && compare list.[r] list.[l] < 0 then r else l
          if compare list.[c] x >= 0 then
            i
          else
            list.[i] <- list.[c]
            loop c
      let i = loop 0
      list.[i] <- x
      list.RemoveAt(this.Count - 1)
      Some min

  static member FromEnumerable(xs, compare) =
    let list = xs |> Seq.sortWith compare |> ResizeArray
    Heap(list, compare)

module Program =
  [<EntryPoint>]
  let main _ =
    let [|n; a; b|] = read id
    let n = int n
    let a = int64 a
    let b = int64 b
    let hs =
      [|
        for _ in 0..(n - 1) ->
          Console.ReadLine() |> int64
      |]

    let negativeCompare l r = - (compare l r)

    let calc () =
      let heap = Heap<_>.FromEnumerable(hs, negativeCompare)
      let rec loop k =
        match heap.TryDequeue() with
        | Some max ->
          // 爆風で受けたダメージの総量。
          let threshold = k * b
          if max <= threshold then
            k
          else
            let max' = max - (a - b)
            if max' > threshold then
              heap.Enqueue(max')
            loop (k + 1L)
        | None ->
          k
      loop 0L

    printfn "%d" (calc ())
    0
