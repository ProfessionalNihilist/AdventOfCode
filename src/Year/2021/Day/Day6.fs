module Year2021.Day6

open AdventOfCode
open System

let ``lanternfish``: Solution = fun (rawInput: string) ->
    let toFish = 
        split ","
        >> Seq.map int64
        >> Seq.groupBy id
        >> Seq.map (fun (f,c) -> f, int64 (Seq.length c))

    let consolidate =
        Seq.groupBy fst
        >> Seq.map (fun (f,c) -> f, Seq.sumBy snd c)

    let spawn (fc: int64 * int64) =
        match fc with
        | 0L,c -> seq { 6L,c; 8L,c }
        | x,c -> seq { x - 1L,c }

    let day = Seq.collect spawn >> consolidate
    let forDays d f = seq { 1 .. d } |> Seq.fold (fun s _ -> day s) f
    let numFish d = toFish >> (forDays d) >> Seq.map snd >> Seq.sum
    
    let part1 = numFish 80 rawInput
    let part2 = numFish 256 rawInput

    part1,part2 