module Year2019.Day6

open AdventOfCode
open System

let test = @"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"

let read (s: string) =
    let a =  s |> Seq.takeWhile ((<>) ')') |> String.Concat
    let b = s.Substring (a.Length + 1)
    a,b

let universalOrbitMap (rawInput: string) =
    let starMap (input: string) = 
        input.Split('\n')
        |> Seq.map read
        |> Seq.groupBy fst
        |> Seq.map (fun (k,o) -> k, o |> Seq.map snd |> Set)
        |> Map.ofSeq

    
    { Part1 = Error "no answer"; Part2 = Error "no answer" }
