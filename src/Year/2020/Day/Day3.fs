module Year2020.Day3

open AdventOfCode
open System

let tobogganTrajectory (rawInput: string) =
    let characterInput = rawInput |> Seq.filter (Char.IsControl >> not)
    let length = rawInput |> Seq.takeWhile (Char.IsControl >> not) |> Seq.length
    let lines = rawInput |> Seq.where (Char.IsControl) |> Seq.length
    let isTree = function | '#' -> true | _ -> false
    let itemAt (x,y) = characterInput |> Seq.item (y * length + x)

    let productOfTrees =
        Seq.map (fun (x,y) -> Seq.initInfinite (fun i -> (x * i) % length, y * i)
            >> Seq.takeWhile (snd >> ((>) lines))
            >> Seq.filter (itemAt >> isTree)
            >> Seq.length >> int64)
        >> Seq.reduce (*)

    let part1 = (productOfTrees [3,1])
    let part2 = (productOfTrees [ 1,1; 3,1; 5,1; 7,1; 1,2 ])

    part1, part2