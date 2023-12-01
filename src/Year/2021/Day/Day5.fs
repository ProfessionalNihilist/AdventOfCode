module Year2021.Day5

open AdventOfCode
open System
open FParsec

type Line = {x1: int; y1: int; x2: int; y2:int}

let ppoint = pint32 .>> skipChar ',' .>>. pint32
let pign = spaces1 .>> skipString "->" .>> spaces1
let pline = ppoint .>> pign .>>. ppoint |>> (fun ((x1,y1),(x2,y2)) -> {x1=x1;y1=y1;x2=x2;y2=y2})
let plines = sepEndBy1 pline newline

let ``hydrothermal venture``: Solution = fun (rawInput: string) ->
    let lines = match run plines rawInput with | Success (l,_,_) -> l | _ -> failwithf "failed"
    let isDiagonal (l: Line) = l.x1 <> l.x2 && l.y1 <> l.y2
    let getPoints (l: Line) = seq {
        if isDiagonal l then
            let dx = Math.Clamp(l.x2 - l.x1, -1, 1)
            let dy = Math.Clamp(l.y2 - l.y1, -1, 1)
            let points = abs (max (abs l.x1 - l.x2) (abs l.y1 - l.y2))
            for p in 0 .. points do yield l.x1 + dx * p, l.y1 + dy * p
        else
            for x in 0 .. abs (l.x1 - l.x2) do
                for y in 0 .. abs (l.y1 - l.y2) do
                    yield (min l.x1 l.x2) + x, (min l.y1 l.y2) + y }

    let dangerous =
        Seq.collect getPoints
        >> Seq.groupBy id
        >> Seq.map (snd >> Seq.length)
        >> Seq.filter ((<) 1)
        >> Seq.length

    let part1 = lines |> Seq.filter (isDiagonal >> not) |> dangerous
    let part2 = dangerous lines

    part1,part2 
