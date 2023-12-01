module Year2023

open AdventOfCode
open System
open Years

let part1 = 0L
let part2 = 0L

let (trebuchet: Solution) = fun (input: string) ->
    let perLine l = 
        String( [| Seq.find (Char.IsDigit) l; Seq.findBack (Char.IsDigit) l |] ) |> int64

    let isDigitWord (window: string) = 
        match window with
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

let register () =
    add 2023 1 trebuchet
