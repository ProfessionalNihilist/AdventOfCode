module Year2019.Day2

open System
open AdventOfCode

type Intcode = int[]
type Opcode = Intcode -> int -> Result<bool * int, string>

module Opcodes =
    let hasParameters (i: Intcode, pc) =
        if i.Length > pc + 2 then None
        else Some "No input values"

    let isValidStore (i: Intcode, pc) =
        let address = i.[pc + 3]
        if address >= 0 || i.Length < address then None
        else Some "Destination address out of bounds"

    let validate (i: Intcode) pc =
        [
            hasParameters
            isValidStore
        ] |> Seq.tryPick (fun f -> f (i,pc))

    let add: Opcode = fun i pc ->
        match validate i pc with
        | Some error -> Error error
        | _ ->
            let o = i.[pc + 3]
            i.[o] <- i.[i.[pc + 1]] + i.[i.[pc + 2]]
            Ok (false, 4)

    let multiply: Opcode = fun i pc ->
        match validate i pc with
        | Some error -> Error error
        | _ ->
            let o = i.[pc + 3]
            i.[o] <- i.[i.[pc + 1]] * i.[i.[pc + 2]]
            Ok (false, 4)

    let exit: Opcode = fun _ _ -> Ok (true, 1)

let Opcodes = [
                (1, Opcodes.add)
                (2, Opcodes.multiply)
                (99, Opcodes.exit)
              ] |> Map.ofList

let parseInput (input: string): Intcode =
    input.Split(",")
        |> Seq.map Int32.Parse
        |> Seq.toArray

let programAlarm rawInput =
    let run (i: int[]) one two =
        i.[1] <- one
        i.[2] <- two

        let mutable pc = 0;
        let mutable exit = false

        while not exit do
            match Opcodes.ContainsKey i.[pc] with
            | false -> failwithf "Unknown opcode '%d'" pc
            | true ->
                match Opcodes.[i.[pc]] i pc with
                | Error message ->
                    failwithf "Validation error: %s" message
                | Ok (finished, inc) ->
                    pc <- pc + inc
                    exit <- finished
        i.[0]

    let i = parseInput rawInput
    let pairs = seq {
        for a in 1 .. 99 do
            for b in 1 .. 99 do
                yield (a,b)
    }

    pairs |> Seq.find (fun (a,b) -> (run (Array.copy i) a b) = 19690720) 
    |> sprintf "Inputs are %A"
    |> ignore
    0L,0L


