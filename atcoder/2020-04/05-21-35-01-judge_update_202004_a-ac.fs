module Program

open System
open System.Collections
open System.Collections.Generic

let read f = Console.ReadLine().Split(' ') |> Array.map f

match read int with
| [|S; L; R|] ->
    let X = S |> max L |> min R
    printfn "%d" X
