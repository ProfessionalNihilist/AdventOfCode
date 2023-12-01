module Year2021.Day10

open AdventOfCode
open System
open FParsec

let ``syntax scoring``: Solution = fun (rawInput: string) ->
    let lines = asLines rawInput

    let pBlock, pRef = createParserForwardedToRef ()
    let pairing l r = between (skipChar l) (skipChar r) (pBlock <|> preturn ())
    let blocks = choice [ pairing '(' ')'; pairing '{' '}'; pairing '[' ']'; pairing '<' '>' ]
    pRef.Value <- skipMany blocks

    let getFirstError code =
        match run (skipManyTill pBlock eof) code with
        | Failure (_,e,_) when e.Position.Column < code.Length ->
            Some code[int e.Position.Index]
        | _ -> None

    let points = function | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137

    let part1 = lines |> Seq.choose getFirstError |> Seq.map points |> Seq.sum
    let part2 = 0

    part1,part2 

