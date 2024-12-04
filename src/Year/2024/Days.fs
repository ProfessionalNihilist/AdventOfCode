module Year2024

open AdventOfCode
open FParsec
open System
open Years
open System.Text.RegularExpressions

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

module Day2 =
    let ``red-nosed reports`` input =
        let preport = sepBy1 pint64 (pchar ' ')
        let parser = sepEndBy1 preport newline

        match run parser input with
        | Failure (a,b,_) -> failwithf "Failed: %s, %A" a b
        | Success (reports,_,_) ->
            let differences = List.pairwise >> List.map (fun (l,r) -> int64 (l - r))
            let withinTolerance = differences >> List.map abs >> List.forall (fun x -> x >= 1L && x <= 3L)

            let isSafe1 report =
                let d  = differences report
                withinTolerance report
                    && d |> (List.map sign >> List.pairwise >> List.forall (fun (l,r) -> l = r))


            let part1 =
                reports
                |> List.filter isSafe1
                |> List.length
                |> int64

            part1,0L

let register () =
    add 2024 1 Day1.``historian hysteria``
    add 2024 2 Day2.``red-nosed reports``

