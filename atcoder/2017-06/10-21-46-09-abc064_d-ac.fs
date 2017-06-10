namespace VainZero.Procon.FSharp

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let read f = Console.ReadLine().Split([|' '|]) |> Array.map f

module Program =
  [<EntryPoint>]
  let main _ =
    let _ = Console.ReadLine()
    let source = Console.ReadLine()

    let calc () =
      let (depth, minDepth) =
        // 括弧の深さ
        let mutable d = 0
        // 括弧の深さの最小値
        let mutable minDepth = Int32.MaxValue
        for i in 0..(source.Length  - 1) do
          d <- d + (if source.[i] = '(' then 1 else -1)
          minDepth <- minDepth |> min d
        (d, minDepth)
      
      // minDepth が負なら括弧列の一部に「閉じすぎ」な部分があるので、
      // 必要なだけ開き括弧を補う。
      // 左端に挿入すれば辞書順最小になる。
      let leftParenthesis =
        if minDepth < 0 then String('(', -minDepth) else ""
      // 左括弧の挿入により、全体の深さが (-minDepth) 増加する。
      let depth =
        if minDepth < 0 then depth - minDepth else depth

      // NOTE: 左括弧の挿入により、全体の深さは 0 以上になる。

      // 右端の深さが 0 より大きいなら、「開きすぎ」なので、閉じ括弧を挿入する。
      // 右端に挿入すれば辞書順最小になる。
      let rightParenthesis =
        if depth > 0 then String(')', depth) else ""

      leftParenthesis + source + rightParenthesis

    printfn "%s" (calc ())

    0
