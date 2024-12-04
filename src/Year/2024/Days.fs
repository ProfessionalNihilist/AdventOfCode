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

module Day4 =
    type Direction = N | NE | E | SE | S | SW | W | NW
    let ``ceres search`` input =
        let grid = input |> asLines |> Array.map (fun s -> s.ToCharArray ())
        let grid = Array2D.init (grid.Length) (grid.[0].Length) (fun x y -> grid.[x].[y])
        let boundsY = (Array2D.length1 grid) - 1
        let boundsX = (Array2D.length2 grid) - 1

        let directions = [| N; NE; E; SE; S; SW; W; NW |]

        let part1 =
            let canSearch x y direction =
                match direction with
                | N  when y >= 3                                -> true
                | NE when y >= 3 && x <= boundsX - 3            -> true
                | E  when x <= boundsX - 3                      -> true
                | SE when x <= boundsX - 3 && y <= boundsY - 3  -> true
                | S  when y <= boundsY - 3                      -> true
                | SW when y <= boundsY - 3 && x >= 3            -> true
                | W  when x >= 3                                -> true
                | NW when x >= 3 && y >= 3                      -> true
                | _                                             -> false

            let search x y direction =
                let isXmas a = "XMAS" = String ( Array.ofSeq a )
                match direction with
                | N  when isXmas (Seq.rev grid[y - 3..y,x])                -> Some (x,y)
                | S  when isXmas grid[y..y+3,x]                            -> Some (x,y)
                | E  when isXmas grid[y,x..x+3]                            -> Some (x,y)
                | W  when isXmas (Seq.rev grid[y,x - 3..x])                -> Some (x,y)
                | SE when isXmas (Array.init 4 (fun n -> grid[y+n,x+n]))   -> Some (x,y)
                | NE when isXmas (Array.init 4 (fun n -> grid[y-n,x+n]))   -> Some (x,y)
                | SW when isXmas (Array.init 4 (fun n -> grid[y+n,x-n]))   -> Some (x,y)
                | NW when isXmas (Array.init 4 (fun n -> grid[y-n,x-n]))   -> Some (x,y)
                | _ -> None

            let mutable xmases = 0L
            for x in 0 .. boundsX do
                for y in 0 .. boundsY do
                    if grid[y,x] = 'X' then
                        for d in directions |> Seq.filter (canSearch x y) do
                            match search x y d with
                            | Some _ -> xmases <- xmases + 1L;
                            | _ -> ()
            xmases

        let part2 =
            let canSearch x y = x > 0 && y > 0 && x < boundsX && y < boundsY
            let masOrSam s = s = "MAS" || s = "SAM"
            let search x y =
                let first = String (Array.init 3 (fun n -> grid[y-1+n,x-1+n]))
                let second = String (Array.init 3 (fun n -> grid[y+1-n,x-1+n]))
                if (masOrSam first && masOrSam second) then Some (x,y) else None

            let mutable mases = 0L
            for x in 0 .. boundsX do
                for y in 0 .. boundsY do
                    if grid[y,x] = 'A' && canSearch x y then
                        match search x y with
                        | Some _ -> mases <- mases + 1L
                        | None -> ()
            mases

        part1,part2

let register () =
    add 2024 1 Day1.``historian hysteria``
    add 2024 2 Day2.``red-nosed reports``
    add 2024 3 Day3.``mull it over``
    add 2024 4 Day4.``ceres search``

