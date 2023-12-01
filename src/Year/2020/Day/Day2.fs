module Year2020.Day2

open AdventOfCode
open System

let passwordPhilosophy (rawInput: string) =
    let input  = rawInput.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    let parseRule (rule: string) =
        let mutable i = 0

        let reader = seq {
            while i < (rule.Length) do
                yield rule.[i]
                i <- i + 1
        }

        let readInt = Seq.takeWhile (Char.IsDigit)
                        >> String.Concat
                        >> int

        let min = readInt reader
        let max = reader |> Seq.skip 1 |> readInt
        let character = reader |> Seq.skip 1 |> Seq.head
        let password = reader |> Seq.skip 3 |> String.Concat
        (min,max,character,password)

    let matchesMinMaxRule (min,max,character,password) =
        let chars = password |> Seq.where ((=) character) |> Seq.length
        chars >= min && chars <= max

    let matchesPositionRule ((p1,p2,character,password): int * int * char * string) =
        match p2 > password.Length with
        | true when p1 > password.Length -> false
        | true -> password.[p1 - 1] = character
        | false ->
            match (password.[p1 - 1] = character), (password.[p2 - 1] = character) with
            | true, true -> false
            | false, false -> false
            | _ -> true

    let validMinMax =
        input
        |> Seq.where (parseRule >> matchesMinMaxRule)
        |> Seq.length
        |> int64

    let validPositional =
        input
        |> Seq.where (parseRule >> matchesPositionRule)
        |> Seq.length
        |> int64

    validMinMax, validPositional
