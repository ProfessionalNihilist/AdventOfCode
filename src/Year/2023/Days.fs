module Year2023

open AdventOfCode
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

let day2 input =

    part1,part2
    
let day3 input = part1,part2
let day4 input = part1,part2
let day5 input = part1,part2
let day6 input = part1,part2
let day7 input = part1,part2
let day8 input = part1,part2
let day9 input = part1,part2

let register () =
    add 2023 1 trebuchet
    // add 2023 2 day2
    // add 2023 3 day3
    // add 2023 4 day4
    // add 2023 5 day5
    // add 2023 6 day6
    // add 2023 7 day7
    // add 2023 8 day8
    // add 2023 9 day9
