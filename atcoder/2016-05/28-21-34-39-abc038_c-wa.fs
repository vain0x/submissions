open System

let c2 n = n * (n - 1) / 2

let n = Console.ReadLine() |> int
let xs = Console.ReadLine().Split(' ') |> Array.map int

type Maximal =
  | Maximal
  | NotMaximal

let rec loop maximal l r =
  if r + 1 < n && xs.[r] < xs.[r + 1] then
    loop Maximal l (r + 1)
  elif l = r && r + 1 < n then
    loop Maximal (l + 1) (r + 1)
  elif l + 1 <= r then
    let k =
      match maximal with
      | Maximal ->
          // (xs.[l], ..., xs.[r]) は狭義単調増加な極大区間
          // これが含む区間の総数は、l..r から2個とる組み合わせの総数に等しい。
          //eprintfn "(%d, %d)" l r
          if maximal = Maximal then c2(r + 1 - l) else 0
      | NotMaximal -> 0
    in
      k + loop NotMaximal (l + 1) r
  else
    // 自明な解((l, r) where l = r)の個数を加えておく
    n

printfn "%d" (loop Maximal 0 0)
