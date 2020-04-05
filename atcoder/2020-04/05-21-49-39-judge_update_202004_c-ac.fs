module Program

open System
open System.Collections
open System.Collections.Generic

let read f = Console.ReadLine().Split(' ') |> Array.map f

let a = read int
let n = a |> Seq.sum

let rev =
    let rev = Dictionary()

    let mutable i = 0
    for y in 0..a.Length - 1 do
        for x in 0..a.[y] - 1 do
            rev.Add(struct (x, y), i)
            i <- i + 1

    rev

let p = [|1..n|]

let at x y = p.[rev.[struct (x, y)]]

let isOk () =
    let mutable ok = true

    for y in 0..a.Length - 1 do
        for x in 0..a.[y] - 1 do
            if y >= 1 && at x y >= at x (y - 1) then
                ok <- false

            if x >= 1 && at x y >= at (x - 1) y then
                ok <- false

    ok

let mutable count = 0

let rec dfs (i: int) =
    if i = n then
        if isOk () then
            count <- count + 1
    else
        for j in i..n - 1 do
            let t = p.[i]
            p.[i] <- p.[j]
            p.[j] <- t

            dfs (i + 1)

            p.[j] <- p.[i]
            p.[i] <- t

dfs 0
printfn "%d" count
