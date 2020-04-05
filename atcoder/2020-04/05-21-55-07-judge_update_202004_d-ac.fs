module Program

open System
open System.Collections
open System.Collections.Generic

let read f = Console.ReadLine().Split(' ') |> Array.map f

let rec gcd (x: int) (y: int) =
    if y = 0 then
        x
    else
        gcd y (x % y)

let N, Q =
    match read int with
    | [|N; Q|] -> N, Q
    | _ -> failwith "bad input"

let A = read int
let S = read int

let n = A.Length

let acc = A |> Array.ofSeq
for i in 1..n - 1 do
    acc.[i] <- acc.[i] |> gcd acc.[i - 1]

for s in S do
    let t =
        let x = s |> gcd acc.[n - 1]
        if x <> 1 then
            x
        else
            let mutable l = -1
            let mutable r = n - 1

            while r - l > 1 do
                let m = (l + r) / 2
                if gcd s acc.[m] = 1 then
                    r <- m
                else
                    l <- m

            r + 1

    printfn "%d" t
