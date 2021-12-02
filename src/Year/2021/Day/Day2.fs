module Year2021.Day2

open AdventOfCode
open System

type Command = Forward of int | Down of int | Up of int

let ``dive!``: Solution = fun (rawInput: string) ->
    let parsed = asLines rawInput |> Seq.map (fun s ->
        let s = s.Split(' ', trimAndEmpty)
        Int32.Parse(s.[1])
        |> match s.[0] with
            | "forward" -> Forward
            | "down" -> Down
            | "up" -> Up
            | _ -> failwithf "unknown command %s" s.[0])

    let (h,d) = parsed |> Seq.fold (fun (h,d) c ->
        match c with
        | Forward x -> h + x, d
        | Down x -> h, d + x
        | Up x -> h, d - x) (0,0)
    let part1 = h * d

    let (h,d,a) = parsed |> Seq.fold (fun (h,d,a) c ->
        match c with
        | Forward x -> h + x, d + (a * x), a
        | Down x -> h, d, a + x
        | Up x -> h, d, a - x) (0,0,0)
    let part2 = h * d

    { Part1 = Ok (sprintf "%d" part1); Part2 = Ok (sprintf "%d" part2) }

