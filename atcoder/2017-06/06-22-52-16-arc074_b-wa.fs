namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

type MultiValueSet<'x> private (dictionary: Dictionary<'x, int>) =
  member this.Add(x) =
    match dictionary.TryGetValue(x) with
    | (true, count) ->
      dictionary.[x] <- count + 1
    | (false, _) ->
      dictionary.Add(x, 1)

  member this.Remove(x) =
    match dictionary.TryGetValue(x) with
    | (true, count) ->
      if count <= 1 then
        dictionary.Remove(x) |> ignore
      else
        dictionary.[x] <- count - 1
      true
    | (false, _) ->
      false

  member this.ToArray() =
    [|
      for KeyValue (x, count) in dictionary do
        for _ in 0..(count - 1) -> x
    |]

  static member Empty() =
    MultiValueSet<'x>(Dictionary<'x, int>(EqualityComparer<'x>.Default))

  static member FromEnumerable(xs: seq<'x>) =
    let set = MultiValueSet<_>.Empty()
    for x in xs do
      set.Add(x)
    set

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

/// 与えられた配列をある位置で2つに分割したときの、左側を表す。
/// その部分配列に含まれる値のうち、上位 n 個だけをヒープとして所有する。
/// それらの合計値もキャッシュする。
/// 分割位置が1つ後方にずらしたとき、左側部分配列に含まれる要素が1増えるので、
/// 必要に応じてヒープと合計値を更新する。更新は O(log n) で行う。
type LeftWing(heap: Heap<int64>) =
  let mutable sum =
    let mutable sum = 0L
    for x in heap.AsList do
      sum <- sum + x
    sum

  member this.Sum = sum

  member this.Add(x) =
    let min = heap.Min
    // 上位 n 項の最小値より真に大きいということは、
    // 追加される値が新たな上位 n 項に加わり、最小値が抜けるということ。
    // (そうでない場合は何もしなくてよい。)
    if x > min then
      heap.Dequeue() |> ignore
      heap.Enqueue(x)
      sum <- sum - min + x

  member this.ToSeq() =
    heap.AsList :> seq<_>

type RightWing(smalls: MultiValueSet<int64>, larges: Heap<int64>, sum) =
  let mutable sum = sum

  /// larges から除去されるべき要素たち
  let removed = MultiValueSet<_>.Empty()

  member this.Sum = sum

  member this.Remove(x) =
    if smalls.Remove(x) then
      let y =
        let rec loop () =
          let y = larges.Dequeue()
          if removed.Remove(y) then
            loop ()
          else
            y
        loop ()
      smalls.Add(y)
      sum <- sum - x + y
    else
      removed.Add(x)

  member this.ToSeq() =
    smalls.ToArray() :> seq<_>

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let n = Console.ReadLine() |> int
    let xs = read int64

    let calc () =
      let leftWing =
        let heap = Heap<_>.FromEnumerable(ArraySegment(xs, 0, n), compare)
        LeftWing(heap)
      let rightWing =
        let ys =
          ArraySegment(xs, n, 2 * n)
          |> Seq.sort
          |> Seq.toArray
        let smalls = ArraySegment(ys, 0, n)
        let sum = smalls |> Seq.sum
        let smalls =
          MultiValueSet<_>.FromEnumerable(smalls)
        let larges =
          Heap<_>.FromEnumerable(ArraySegment(ys, n, n), compare)
        RightWing(smalls, larges, sum)

      let score () = leftWing.Sum - rightWing.Sum
      let mutable maxScore = score ()

      let print i =
        let xs = xs |> Seq.map string
        eprintfn "[%d] %s | %s" i (xs |> Seq.take i |> String.concat " ") (xs |> Seq.skip i |> String.concat " ")
        eprintfn "LW (%d): {%s}" leftWing.Sum (leftWing.ToSeq() |> Seq.map string |> String.concat " ")
        eprintfn "RW (%d): {%s}" rightWing.Sum (rightWing.ToSeq() |> Seq.map string |> String.concat " ")

      for i in n..(2 * n - 1) do
        //print i
        let x = xs.[i]
        leftWing.Add(x)
        rightWing.Remove(x)
        maxScore <- max maxScore (score ())

      //print (2 * n)
      maxScore

    printfn "%d" (calc ())

    0
