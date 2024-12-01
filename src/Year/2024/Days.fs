module Year2024

open AdventOfCode
open FParsec
open System
open Years

module Day1 =
    let ``historian hysteria`` input =
        let parser = sepEndBy (pint64 .>> spaces .>>. pint64) newline
        match run parser input with
        | Failure (a,b,_) -> failwithf "Failed: %s, %A" a b
        | Success (lists,_,_) ->
            let left,right = lists |> (Array.ofList >> Array.unzip)

            let part1 = Seq.map2
                            (fun l r -> abs (l - r)) 
                            (Seq.sort left) 
                            (Seq.sort right)
                        |> Seq.sum

            let occurrences l = Seq.length (Seq.filter ((=) l) right) |> int64
            let part2 =
                left
                |> Seq.map (fun l -> l * occurrences l)
                |> Seq.sum

            part1,part2

let register () =
    add 2024 1 Day1.``historian hysteria``

