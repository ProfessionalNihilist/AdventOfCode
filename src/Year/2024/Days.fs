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

module Day3 =
    type State = { Pairs: (int * int) list; Mul: bool }
        with static member Initial = { Pairs = []; Mul = true }

    let ``mull it over`` input =
        let matches = Regex.Matches (input, @"mul\((\d{1,3}),(\d{1,})\)")
        let part1 = matches |> Seq.map (fun x -> x.Groups.Item(1).Value, x.Groups.Item(2).Value)
                            |> Seq.sumBy (fun (l,r) -> int64 l * int64 r)

        let doMul = pstring "do()" .>> updateUserState (fun s -> true) >>% None
        let dontMul = pstring "don't()" .>> updateUserState (fun s -> false) >>% None
        let pmul = pstring "mul(" >>. pint64 .>> pchar ',' .>>. pint64 .>> pchar ')' .>> userStateSatisfies id |>> Some
        let parser = many ((attempt pmul) <|> doMul <|> dontMul <|> (anyChar >>% None))
        let part2 = match runParserOnString parser true "" input with
                    | Success (r,_,_) -> r |> Seq.choose id |> Seq.sumBy (fun (l,r) -> l * r) |> int64
                    | Failure _ -> 0L
        part1,part2

let register () =
    add 2024 1 Day1.``historian hysteria``
    add 2024 2 Day2.``red-nosed reports``
    add 2024 3 Day3.``mull it over``

