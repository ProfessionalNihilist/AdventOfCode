module Day2

open System

type Intcode = int[]
type Opcode = Intcode -> int -> Result<bool, string>

module private Opcodes =
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
            Ok false

    let multiply: Opcode = fun i pc ->
        match validate i pc with
        | Some error -> Error error
        | _ ->
            let o = i.[pc + 3]
            i.[o] <- i.[i.[pc + 1]] * i.[i.[pc + 2]]
            Ok false

    let exit: Opcode = fun _ _ -> Ok true

let Opcodes = [
                (1, Opcodes.add)
                (2, Opcodes.multiply)
                (99, Opcodes.exit)
              ] |> Map.ofList

let parseInput (input: string): Intcode =
    input.Split(",")
        |> Seq.map Int32.Parse
        |> Seq.toArray

let runProgram (getInput: unit -> string) =
    let i = parseInput (getInput())

    // manual correction as specified
    i.[1] <- 12
    i.[2] <- 2

    let mutable pc = 0;
    let mutable exit = false

    while not exit do
        match Opcodes.ContainsKey i.[pc] with
        | false -> failwithf "Unknown opcode '%d'" pc
        | true ->
            match Opcodes.[i.[pc]] i pc with
            | Error message ->
                failwithf "Validation error: %s" message
            | Ok e ->
                pc <- pc + 4
                exit <- e

    printfn "Program result is %d" i.[0]

