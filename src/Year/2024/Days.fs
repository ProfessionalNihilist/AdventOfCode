module Year2024

open AdventOfCode
open FParsec
open System
open Years
open System.Text.RegularExpressions

type Direction = N | NE | E | SE | S | SW | W | NW

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
            let changes = List.pairwise >> List.map (fun (l,r) -> int64 (abs (l - r)), int64 (sign (l - r)))

            let isSafe1 report =
                let c = changes report
                c |> List.forall (fun (x,_) -> x >= 1 && x <= 3)
                    && c |> (List.countBy snd >> List.length >> (=) 1)

            let isSafe2 report =
                report  |> List.mapi (fun i _ -> List.removeAt i report)
                        |> List.exists isSafe1

            let countSafe f =
                reports |> List.filter f |> List.length |> int64

            let part1 = countSafe isSafe1
            let part2 = countSafe isSafe2

            part1,part2

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

module Day5 =
    let ``print queue`` input =
        let rules = many (pint64 .>> pchar '|' .>>. pint64 .>> newline) .>> newline
        let pages = sepEndBy1 (sepBy1 pint64 (pchar ',')) newline

        let inOrder pages (r1,r2) =
            match List.tryFindIndex ((=) r1) pages, List.tryFindIndex ((=) r2) pages with
            | Some i1, Some i2 -> i1 < i2
            | _ -> true
            
        let middle (x: 'a list) =
            let l = List.length x
            let mi = floor (float l / 2.0)
            List.item (int mi) x

        let (rules,pages) = parseOrThrow (rules .>>. pages) input

        let ordered,unordered = List.partition (fun p -> List.forall (inOrder p) rules) pages

        let sumMiddles = List.map middle >> List.sum >> int64

        let before l r =
            match List.tryFindIndex ((=) (l,r)) rules with
            | Some _ -> -1
            | None ->
                match List.tryFindIndex ((=) (r,l)) rules with
                | Some _ -> 1
                | None -> 0

        let reordered = List.map (List.sortWith before) unordered

        sumMiddles ordered, sumMiddles reordered

module Day6  =
    let ``guard gallivant`` input =
        let grid = Array2D.ofString input
        let xy = grid |> Array2D.picki (fun xy i -> if i = '^' then Some xy else None)

        let boundsX = Array2D.length1 grid - 1
        let boundsY = Array2D.length2 grid - 1

        let isInBounds x y =
            x >= 0 && y >= 0 && x <= boundsX && y <= boundsY

        let walk origin (tf: int -> bool) (p: (int*int) -> unit) (g: char [,])  =
            let mutable inBounds = true
            let mutable looped = false
            let mutable facing = N
            let mutable x = fst origin
            let mutable y = snd origin

            let frequency = Array2D.init (Array2D.length1 g) (Array2D.length2 g) (fun _ _ -> 0)
            frequency[y,x] <- frequency[y,x] + 1

            while inBounds && not looped do
                looped <- tf frequency[y,x]
                g[y,x] <- 'X'
                let nx,ny = match facing with
                            | N -> x, y - 1
                            | S -> x, y + 1
                            | E -> x + 1, y
                            | W -> x - 1, y

                match isInBounds nx ny with
                | true when g[ny,nx] = '#' || g[ny,nx] = 'O' ->
                    facing <- match facing with | N -> E | E -> S | S -> W | W -> N
                | true ->
                    p (nx,ny)
                    frequency[y,x] <- frequency[y,x] + 1
                    x <- nx
                    y <- ny
                | false -> inBounds <- false
            g,looped

        let mutable path = []
        let addToPath xy =
            path <- xy :: path
            () 

        let p1,_ = walk xy (fun _ -> false) addToPath (Array2D.copy grid)
        let part1 = Array2D.countBy ((=) 'X') p1 |> int64

        // ok lets brute-force this mother
        let clones = path |> Seq.filter ((<>) xy)
                          |> Seq.distinct
                          |> Seq.map (fun (x,y) ->  let g = Array2D.copy grid
                                                    g[y,x] <- 'O'
                                                    g)

        let hasLooped s = s > 10
        let part2 = clones  |> Seq.map (fun c -> walk xy hasLooped (fun _ -> ()) c)
                            |> Seq.map snd
                            |> Seq.filter id
                            |> Seq.length
                            |> int64

        part1,part2

module Day7 =
    let ``bridge repair`` input =
        let ptest = pint64 .>> pchar ':' .>> spaces
        let pvalues = (sepBy pint64 (pchar ' ')) .>> spaces
        let pcalibrations = many (ptest .>>. pvalues)

        let solve operators (test, values) =
            let rec apply sum vals =
                match vals with
                | i :: tail -> operators |> List.tryPick (fun op -> apply (op sum i) tail)
                | [] -> if sum = test then Some test else None
            apply (List.head values) (List.tail values)

        let solve1 = solve [(+); (*)]
        let solve2 = solve [(+); (*); (fun l r -> int64 (sprintf "%d%d" l r))]

        let calibrations = parseOrThrow pcalibrations input

        let part1 =
            calibrations
            |> List.choose solve1
            |> List.sum

        let part2 =
            calibrations
            |> List.choose solve2
            |> List.sum

        part1,part2

let register () =
    add 2024 1 Day1.``historian hysteria``
    add 2024 2 Day2.``red-nosed reports``
    add 2024 3 Day3.``mull it over``
    add 2024 4 Day4.``ceres search``
    add 2024 5 Day5.``print queue``
    add 2024 6 Day6.``guard gallivant``
    add 2024 7 Day7.``bridge repair``

