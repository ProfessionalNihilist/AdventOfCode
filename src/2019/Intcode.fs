module Intcode
open System

type ExecutionResult<'a> = Cont of 'a | Halt

type ParameterMode =
    | Position
    | Immediate with
    static member Parse (i: int) =
        match i with
        | 0 -> Position
        | 1 -> Immediate
        | _ -> failwithf "Unknown parameter mode %d" i

type DecodedOpcode =
    | Binary of Operation: (int -> int -> int) * ParameterMode * ParameterMode
    | Input
    | Output
    | Jump of (int -> bool) * ParameterMode * ParameterMode
    | Compare of (int -> int -> bool) * ParameterMode * ParameterMode
    | Exit

type NextInstruction = Increment of int | GoTo of int

let decode (opcode:int) =
    let p3 = (opcode / 10000) % 10 |> ParameterMode.Parse
    let p2 = (opcode / 1000) % 100 % 10 |> ParameterMode.Parse
    let p1 = (opcode / 100) % 1000 % 100 % 10 |> ParameterMode.Parse
    let opcode = opcode % 10000 % 1000 % 100

    match opcode with
    | 1 -> Binary  ((+), p1, p2)
    | 2 -> Binary ((*), p1, p2)
    | 3 -> Input
    | 4 -> Output
    | 5 -> Jump ((<>) 0, p1, p2)
    | 6 -> Jump ((=) 0, p1, p2)
    | 7 -> Compare ((<), p1, p2)
    | 8 -> Compare ((=), p1, p2)
    | 99 -> Exit
    | _ -> failwithf "Unknown opcode %d" opcode

let getParameter (i: int[]) a mode =
    match mode with
    | Position -> i.[a]
    | Immediate -> a

let executeStep
    (output: int -> unit)
    (input: unit -> int)
    (state: int[] * int)
    (opcode: DecodedOpcode) =
    let i, pc = state
    match opcode with
    | Binary (op, p1, p2) ->
        let l = p1 |> getParameter i (i.[pc + 1])
        let r = p2 |> getParameter i (i.[pc + 2])
        i.[i.[pc + 3]] <- op l r
        Cont (Increment 4)
    | Input ->
        i.[i.[pc + 1]] <- input ()
        Cont (Increment 2)
    | Output ->
        output (i.[i.[pc + 1]])
        Cont (Increment 2)
    | Jump (test, p1, p2) ->
        let v = p1 |> getParameter i (i.[pc + 1])
        let pc = p2 |> getParameter i (i.[pc + 2])
        match test v with
        | true -> Cont (GoTo pc)
        | false -> Cont (Increment 3)
    | Compare (test, p1, p2) ->
        let l = p1 |> getParameter i (i.[pc + 1])
        let r = p2 |> getParameter i (i.[pc + 2])
        i.[i.[pc + 3]] <- if test l r then 1 else 0
        Cont (Increment 4)
    | Exit -> Halt

let parseInput (input: string): int[]=
    input.Split(",")
        |> Seq.map Int32.Parse
        |> Seq.toArray