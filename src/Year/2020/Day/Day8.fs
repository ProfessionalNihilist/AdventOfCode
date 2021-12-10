module Year2020.Day8

open AdventOfCode
open System
open System.Collections.Generic
open FParsec

type Ast = | Nop of int | Acc of int | Jmp of int

let handheldHalting input =
    let value = (many1Chars2 (anyOf ['-';'+']) digit .>> (optional skipNewline))
    let nop = pstring "nop " >>. value |>> (Int32.Parse >> Nop)
    let acc = pstring "acc " >>. value |>> (Int32.Parse >> Acc)
    let jmp = pstring "jmp " >>. value |>> (Int32.Parse >> Jmp)
    let instruction = choice [nop; acc; jmp]
    let pAst = manyTill instruction eof

    let findLoop ast =
        let ast = Array.ofList ast
        let seen = HashSet<int>()
        let mutable pc = 0
        let mutable acc = 0

        while seen.Add pc do
            match ast.[pc] with
            | Acc a -> 
                acc <- acc + a
                pc <- pc + 1
            | Nop _ -> pc <- pc + 1
            | Jmp j -> pc <- pc + j
        acc

    let findTerminalAcc ast =
        let ast = Array.ofList ast
        let seen = HashSet<int>()
        let mutable pc = 0
        let mutable acc = 0
        let mutable mutationIndex = 0

        while pc <> (ast.Length) do
            pc <- 0
            acc <- 0
            seen.Clear()
            let mutable mutated = false

            while pc <> (ast.Length) && seen.Add pc  do
                match ast.[pc] with
                | Acc a -> 
                    acc <- acc + a
                    pc <- pc + 1
                | i when not mutated && (seen.Count >= mutationIndex + 1) ->
                    mutationIndex <- seen.Count
                    mutated <- true
                    match i with | Nop j -> pc <- pc + j | Jmp _ -> pc <- pc + 1
                | Nop _ -> pc <- pc + 1
                | Jmp j -> pc <- pc + j
        acc

    match run pAst input with
    | Failure (error, _, _) -> failwith error
    | Success (result, _, _) ->
        { Part1 = sprintf "acc before loop is %d" (findLoop result)
          Part2 = sprintf "acc after termination is %d" (findTerminalAcc result) }

