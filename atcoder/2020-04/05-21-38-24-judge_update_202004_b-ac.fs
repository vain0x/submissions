module Program

open System
open System.Collections
open System.Collections.Generic

let read f = Console.ReadLine().Split(' ') |> Array.map f

match read int with
| [|N|] ->
    let red = ResizeArray()
    let blue = ResizeArray()

    for _ in 1..N do
        match read id with
        | [|X; C|] ->
            let X = int X
            if C = "R" then
                red.Add(X)
            else
                blue.Add(X)

        | _ -> failwith "bad input"

    red.Sort()
    blue.Sort()

    for x in Seq.append red blue do
        printfn "%d" x

| _ -> failwith "bad input"
