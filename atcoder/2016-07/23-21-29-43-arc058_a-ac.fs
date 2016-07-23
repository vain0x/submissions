namespace AdhocFs

open System

module Program =
  [<EntryPoint>]
  let main argv =

    let [|s; _|] = Console.ReadLine().Split([|' '|])
    let hatedDigits = Console.ReadLine().Split([|' '|]) |> Array.map int

    let digits = Set.difference (set [0..9]) (hatedDigits |> Set.ofArray)

    // 使える数字のうち、d を超える最小のもの
    let minDigitOver d =
      let largeDigits = digits |> Set.filter (fun d' -> d' > d)
      if largeDigits |> Set.isEmpty
      then None
      else Some (largeDigits |> Set.minElement)

    let minPositiveDigit = minDigitOver 0 |> Option.get

    let minDigit = digits |> Set.minElement

    let solve () =
      /// 同じ桁数で金額より大きい数値の構成を試みる。
      let rec loop acc isLarge i =
        if i = s.Length
        then acc |> List.rev |> List.map string |> String.concat "" |> int |> Some
        else
          if isLarge then
            loop (minDigit :: acc) true (i + 1)
          else
            let d = s.[i] |> string |> int
            if digits |> Set.contains d then
              loop (d :: acc) isLarge (i + 1)
            else
              match minDigitOver d with
              | Some d' -> loop (d' :: acc) true (i + 1)
              | None -> None
      match loop [] false 0 with
      | Some n -> n
      | None ->
        // 金額より1桁大きい数値で最小のものを構成する。
        string minPositiveDigit + String.replicate s.Length (string minDigit)
        |> int

    printfn "%d" (solve ())

    // exit code
    0
