module Year2021.Day4

open AdventOfCode
open System
[<Struct>] type Num = { Value : int; mutable Marked: bool }
type Card = Num[,]

let ``giant squid``: Solution = fun (rawInput: string) ->
    let numbers = (asLines rawInput).[0].Split(",", trimAndEmpty) |> Array.map int
    let lineToNumbers (l : string) = l.Split(" ", trimAndEmpty) |> Array.map int

    let block2Card (block: int[]) =
        Array2D.init 5 5 (fun x y -> { Value = block.[x + y * 5]; Marked = false })

    let blocks = rawInput.Split("\n\n")
                |> Seq.tail
                |> Seq.map (asLines >> Seq.collect lineToNumbers >> Seq.toArray)
                |> Seq.map block2Card
                |> Seq.toArray

    let marked (n: Num) = n.Marked

    let isSolved (c: Num[,]) =
        let indices = seq { 0 .. 4 }
        let solvedX = Seq.tryFind (fun y -> c.[*, y] |> Seq.forall marked) indices
        let solvedY () = Seq.tryFind (fun x -> c.[x, *] |> Seq.forall marked) indices
        Option.bind (fun _ -> Some c) (Option.orElseWith solvedY solvedX)

    let update (c: Num[,]) i =
        Array2D.iteri (fun x y n -> if n.Value = i then c.[x,y].Marked <- true) c

    let pickFirstWinner bs ns = Seq.pick (fun v ->
        Seq.iter (fun b -> update b v) bs
        Option.bind (fun o -> Some (v,o)) (Seq.tryPick isSolved bs)) ns

    let pickLastWinner bs ns = Seq.pick (fun v ->
        Seq.iter (fun b -> update b v) bs
        let unsolved = Seq.filter (isSolved >> Option.isNone) bs
        Option.bind (fun o -> Some( v,o)) (Seq.tryExactlyOne unsolved)) ns

    let score v = Seq.filter (marked >> not) >> Seq.sumBy (fun n -> n.Value) >> ((*) v)

    let (value, winner) = pickFirstWinner blocks numbers
    let part1 = winner |> Array2D.flatten |> (score value)

    let (lv, lw) = pickFirstWinner [(snd (pickLastWinner blocks numbers))] numbers
    let part2 = lw |> Array2D.flatten |> (score lv)

    part1,part2 
