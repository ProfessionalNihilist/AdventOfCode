module Year2023

open AdventOfCode
open FParsec
open System
open Years

let part1 = 0L
let part2 = 0L

let trebuchet input =
    let perLine l = 
        String( [| Seq.find (Char.IsDigit) l; Seq.findBack (Char.IsDigit) l |] ) |> int64

    let isDigitWord: string -> char option = function
        | w when w.StartsWith "one" -> Some '1'
        | w when w.StartsWith "two" -> Some '2'
        | "three" -> Some '3'
        | w when w.StartsWith "four" -> Some '4'
        | w when w.StartsWith "five" -> Some '5'
        | w when w.StartsWith "six" -> Some '6'
        | "seven" -> Some '7'
        | "eight" -> Some '8'
        | w when w.StartsWith "nine" -> Some '9'
        | _ -> None
    
    let isDigit (window: char array) =
        if Char.IsDigit window.[0] then Some window.[0]
        else isDigitWord ( String window )

    let digitsForLine (line: char seq) = 
        let line = Array.ofSeq line
        line
            |> Array.mapi (fun i _ -> isDigit (line.[i..i+4]))
            |> Array.choose id

    let input = asLines input 
    let part1 = input |> Seq.sumBy perLine
    let part2 = input |> Seq.map digitsForLine |> Seq.sumBy perLine

    part1, part2

module Day2 =
    type CubeColour = Red | Green | Blue
    type Game = { Id: int64; Rounds: (int64 * int64 * int64) list }

    let ``cube conundrum`` input =
        let parseRounds (cubes: (int64 * CubeColour) list) =
                cubes |> List.fold
                    (fun (ar, ag, ab) set ->
                        match set with
                        | c,Red     -> ar + c, ag, ab
                        | c,Green   -> ar, ag + c, ab
                        | c,Blue    -> ar, ag, ab + c)
                     (0L,0L,0L) 

        let createGame (id, rounds) =
            let rgbs = rounds |> List.map parseRounds
            { Id = id; Rounds = rgbs; }

        let pDigits = many1Satisfy isDigit
        let pCubeColour = choice [
            pstring "red"   >>% Red
            pstring "green" >>% Green
            pstring "blue"  >>% Blue
        ]
        let pRound = sepBy1 (pDigits |>> int64 .>> spaces1 .>>. pCubeColour) (pchar ',' .>> spaces1)
        let pGameId = skipString "Game" .>> spaces1 >>. pDigits .>> pchar ':' .>> spaces1 |>> int64
        let pGames = sepEndBy1 (pGameId .>>. sepBy1 pRound (pchar ';' .>> spaces1) |>> createGame) newline

        match run pGames input with
        | Failure (a,b, _) -> failwithf "Failed: %s, %A" a b
        | Success (games,_,_) ->
            let part1 = games 
                        |> List.filter (fun game ->
                                game.Rounds 
                                    |> List.exists (fun (r,g,b) -> r > 12 || g > 13 || b > 14)
                                    |> not)
                        |> List.sumBy (fun g -> g.Id)

            let part2 = games
                        |> List.map (fun g ->
                            let (minR, minG, minB) = List.reduce (fun (lr, lg, lb) (rr, rg, rb) -> max lr rr, max lg rg, max lb rb) g.Rounds
                            minR * minG * minB)
                        |> List.sum

            part1,part2
    
module Day3 =
    type PartNumber = Position * int64
    type Element = Symbol of Position * char | Part of PartNumber | Void

    let ``gear ratios`` input =
        let pPartNumber = getPosition .>>. (many1Satisfy isDigit |>> int64) |>> Part
        let pSymbol = getPosition .>>. satisfy (fun c -> c <> '.' && isDigit c |> not) |>> Symbol
        let noise = many (choice [skipChar '.'; skipNewline])
        let parser = (attempt noise) >>. (sepEndBy (pPartNumber <|> pSymbol) noise) .>> eof

        match run parser input with
        | Failure (a,b, _) -> failwithf "Failed: %s, %A" a b
        | Success (elements,a,b) -> 
            let parts = List.choose (function | Part p -> Some p | _ -> None) elements
            let symbols = List.choose (function | Symbol (p,c) -> Some (p,c) | _ -> None) elements

            let neighbouring (ox,oy) (tx,ty) =
                match abs (ox - tx), abs (oy - ty) with
                | 1L, 0L 
                | 0L, 1L 
                | 1L, 1L -> true 
                | _ -> false

            let isAdjacentTo ((pPart, number): PartNumber) (pSym: Position, sym) =
                let partCoords = Seq.init ((string number).Length) (fun i -> pPart.Column + int64 i, pPart.Line)
                Seq.exists (neighbouring (pSym.Column, pSym.Line)) partCoords

            let part1 = 
                parts
                |> Seq.filter (fun p -> List.exists (isAdjacentTo p) symbols)
                |> Seq.sumBy (fun p -> snd p)

            let part2 =
                symbols 
                |> Seq.filter (snd >> ((=) '*'))
                |> Seq.map (fun g -> parts |> Seq.filter (fun p -> isAdjacentTo p g))
                |> Seq.filter (Seq.length >> (=) 2)
                |> Seq.map (fun x -> (Seq.head x |> snd) * (Seq.last x |> snd))
                |> Seq.sum

            part1,part2
let day4 input = part1,part2
let day5 input = part1,part2
let day6 input = part1,part2
let day7 input = part1,part2
let day8 input = part1,part2
let day9 input = part1,part2

let register () =
    add 2023 1 trebuchet
    add 2023 2 Day2.``cube conundrum``
    add 2023 3 Day3.``gear ratios``
    // add 2023 4 day4
    // add 2023 5 day5
    // add 2023 6 day6
    // add 2023 7 day7
    // add 2023 8 day8
    // add 2023 9 day9
