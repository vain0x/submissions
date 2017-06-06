namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

type Heap<'x> (list: ResizeArray<'x>, compare: 'x -> 'x -> int) =
  member this.Count = list.Count
 
  member this.Min =
    list.[0]
 
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
 
  member this.Dequeue() =
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
    min

  member this.AsList =
    list :> IReadOnlyList<_>
 
  static member FromEnumerable(xs, compare) =
    let list = xs |> Seq.sortWith compare |> ResizeArray
    Heap(list, compare)

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let n = Console.ReadLine() |> int
    let xs = read int64

    let negativeCompare l r = - (compare l r)

    let calc () =
      let solve compare (xs: array<int64>) =
        [|
          let heap = Heap<_>.FromEnumerable(ArraySegment(xs, 0, n), compare)
          let mutable sum = heap.AsList |> Seq.sum
          yield sum
          for i in n..(2 * n - 1) do
            let x = xs.[i]
            heap.Enqueue(x)
            let y = heap.Dequeue()
            sum <- sum - y + x
            yield sum
        |]

      let positives = solve compare xs
      let negatives = solve negativeCompare (xs |> Array.rev) |> Array.rev

      seq {
        for i in 0..n ->
          positives.[i] - negatives.[i]
      } |> Seq.max

    printfn "%d" (calc ())

    0
