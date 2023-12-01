module Year2021.Day1

open AdventOfCode
open System

let ``sonar sweep``: Solution = fun (rawInput: string) ->
    let parsed = asLines rawInput |> Seq.map Int32.Parse

    let part1 =
        parsed
        |> Seq.pairwise
        |> Seq.filter (fun (l, r) -> r - l > 0)
        |> Seq.length

    let part2 =
        parsed
        |> Seq.windowed 3
        |> Seq.map Array.sum
        |> Seq.pairwise
        |> Seq.filter (fun (l, r) -> r - l > 0)
        |> Seq.length

    part1,part2 

