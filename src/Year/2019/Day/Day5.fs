module Year2019.Day5

open Intcode
open AdventOfCode

let ``sunny with a chance of asteroids`` (rawInput: string)  =
    let run (intcode: int[]) =
        let mutable pc = 0;
        let mutable exit = false
        let step = executeStep
                    (printfn "Program output: %d")
        let input = fun () -> 5

        while not exit do
            let opcode = decode (intcode.[pc])
            match step input (intcode,pc) opcode with
            | Cont (Increment inc) -> pc <- pc + inc
            | Cont (GoTo goto) -> pc <- goto
            | Halt -> exit <- true
        intcode.[0]

    let input = Day2.parseInput rawInput
    0L, run input |> int64