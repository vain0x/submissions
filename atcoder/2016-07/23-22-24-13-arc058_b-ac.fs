namespace AdhocFs

open System

module Program =
  let modulo = 1000000007L
  let maxH = 100000
  let maxW = 100000

  module Field =
    let p = modulo

    let add x y =
      (x + y) % p

    let mul x y =
      (x * y) % p

    let pow x n =
      assert (n >= 0L)
      let rec loop x n =
        if n = 0L then
          1L
        elif n = 1L then
          x
        elif n % 2L = 0L then
          loop (mul x x) (n / 2L)
        else
          mul x (loop x (n - 1L))
      in loop x n

    let inverse x =
      pow x (p - 2L)

    /// facts.[i] = i!
    /// factInverses.[i] = (i!)^(-1)
    let (facts, factInverses) =
      let length = (maxH + maxW) + 1
      let fs = Array.zeroCreate length
      let fis = Array.zeroCreate length

      // 0! = 1
      fs.[0] <- 1L
      fis.[0] <- 1L

      // (i + 1)! = i! * (i + 1)
      for i in 0..(length - 2) do
        fs.[i + 1] <- fs.[i] |> mul ((i + 1) |> int64)
        fis.[i + 1] <- fs.[i + 1] |> inverse

      (fs, fis)

    let fact n =
      facts.[n]

    let factInverse n =
      factInverses.[n]

    let rec combi n r =
      if n < 0 || not (0 <= r && r <= n) then
        0L
      elif r > n / 2 then
        combi n (n - r)
      else
        fact n
        |> mul (factInverse (n - r))
        |> mul (factInverse r)

  [<EntryPoint>]
  let main argv =
    // h * w の矩形の左上マスから右下マスへ行く方法の個数 (mod modulus)
    let countWays h w =
      let y = h - 1
      let x = w - 1
      in Field.combi (y + x) y

    let [|h; w; a; b|] = Console.ReadLine().Split([|' '|]) |> Array.map int

    // マス (y, x) を通って右下へ行く経路の個数
    let countWaysVia y x =
      countWays (y + 1) (x + 1)
      |> Field.mul (countWays (h - y) (w - x))
      
    let solve () =
      let rec loop acc i =
        let y = h - 1 - a - i
        let x = b + i
        if not (0 <= y && x < w) then
          acc
        else
          loop (Field.add acc (countWaysVia y x)) (i + 1)
      in loop 0L 0

    printfn "%d" (solve ())

    // exit code
    0
