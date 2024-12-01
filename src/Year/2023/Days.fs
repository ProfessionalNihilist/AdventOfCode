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

        let pCubeColour = choice [
            pstring "red"   >>% Red
            pstring "green" >>% Green
            pstring "blue"  >>% Blue
        ]
        let pRound = sepBy1 (pint64 .>> spaces1 .>>. pCubeColour) (pchar ',' .>> spaces1)
        let pGameId = skipString "Game" .>> spaces1 >>. pint64 .>> pchar ':' .>> spaces1
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
        let pPartNumber = getPosition .>>. pint64 |>> Part
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

module Day4 =
    let scratchcards input =
        let pNum = pint64 .>> spaces
        let pWinning = pstring "Card" .>> spaces1 >>. many1 digit .>> pchar ':' .>> spaces >>. manyTill pNum (pchar '|')
        let pOwn = spaces >>. many1 pNum 
        let parser = many ( pWinning .>>. pOwn ) .>> eof

        match run parser input with
        | Failure (a,b, _) -> failwithf "Failed: %s, %A" a b
        | Success (games,a,b) -> 
            let intersections (w: int64 list,o: int64 list) =
                Set.intersect (Set.ofList w) (Set.ofList o) |> Set.count

            let part1 =
                games
                |> List.map intersections
                |> Seq.filter ((<) 0)
                |> Seq.map (fun x -> pown 2 (x - 1) |> int64)
                |> Seq.sum

            let scoredGames =
                games
                |> Seq.map intersections
                |> Seq.map (fun x -> x, ref 1)
                |> Array.ofSeq
            
            let length = Array.length scoredGames
            for index = 0 to length - 1 do
                let matching,numberOfCards = scoredGames.[index]
                for _ = 1 to numberOfCards.Value do
                    for i = 1 to matching do
                        if index + i < length then
                            let cell = snd scoredGames.[index + i] 
                            cell.Value <- cell.Value + 1

            let part2 = Array.sumBy (fun (_,y: ref<int>) -> y.Value) scoredGames |> int64
            part1, part2

module Day5 =
    let ``if you give a seed a fertilizer`` input =
        let pName = many1Satisfy isAsciiLetter  
        let pCategoryName = pName .>> pstring "-to-" .>>. pName .>> pstring " map:" .>>  newline
        let pRanges = sepEndBy1 (tuple3 (pint64 .>> pchar ' ') (pint64 .>> pchar ' ') pint64) newline 
        let pCategoryMap = pCategoryName .>>. pRanges  
        let pSeeds = pstring "seeds: " >>. sepBy1 pint64 (pchar ' ')
        let parser = pSeeds .>> (many1 newline) .>>. many (pCategoryMap .>> spaces) .>> eof

        let toMappingFuncs (_,values) =
            let mapper (dest, source, length) x =
                match x >= source && x < source + length with
                | false -> None
                | true  -> 
                let a = x + (dest - source) |> Some
                a

            let funcs = 
                Seq.append (values 
                            |> Seq.sortBy (fun (_,x,_) -> x)
                            |> Seq.map  mapper)
                            [Some]
                            |> Seq.toArray

            fun x -> Array.pick (fun y -> y x) funcs

        match run parser input with
        | Failure (a,b, _) -> failwithf "Failed: %s, %A" a b
        | Success ((seeds, mappings),a,b) ->
            let combined = 
                mappings 
                |> Seq.map toMappingFuncs
                |> Seq.reduce (>>)

            let part1 = seeds |> Seq.map combined |> Seq.min

            failwithf "part2 doesn't work"

            let part2 = seeds
                        |> Seq.pairwise 
                        |> Seq.collect (fun (seed,length) -> seq { for i in seed..seed + length -> i })
                        |> Seq.map combined
                        |> Seq.min

            part1,part2

module Day6 =
    let ``wait for it`` input = 
        let toNumbers =
            split " " >> Array.skip 1 >> Array.map (fun x -> x.Trim()) >> Array.map int64

        let times = (input |> asLines).[0] |> toNumbers
        let distances = (input |> asLines).[1] |> toNumbers

        let winningTimes t d =
            Seq.init (int t) int64
            |> Seq.filter (fun x -> (t - x) * x > d)

        let part1 = Array.map2 winningTimes times distances 
                    |> Array.map Seq.length 
                    |> Array.reduce (*)
                    |> int64

        let join = Array.map string >> Array.reduce (+) >> int64
        let time = join times
        let distance = join distances

        let part2 = winningTimes time distance |> Seq.length |> int64

        part1,part2
        
module Day7 =
    let day7 input = 
        let example = """"""
        part1,part2

module Day8 =
    let day8 input = part1,part2

module Day9 =
    let day9 input = part1,part2

let register () =
    add 2023 1 trebuchet
    add 2023 2 Day2.``cube conundrum``
    add 2023 3 Day3.``gear ratios``
    add 2023 4 Day4.scratchcards
    add 2023 5 Day5.``if you give a seed a fertilizer``
    add 2023 6 Day6.``wait for it``
    add 2023 7 Day7.day7
    // add 2023 8 day8
    // add 2023 9 day9
