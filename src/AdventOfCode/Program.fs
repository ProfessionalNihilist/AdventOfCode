open System
open FSharp.Data

let days = [
    (1, Day1.fuelCalculator)
    (2, Day2.runProgram)
           ] |> Map.ofList

[<EntryPoint>]
let main argv =
    let rec waitForInput () =
        let input = Console.ReadLine()

        match input with
        | "q" -> ()
        | _ ->
            match Int32.TryParse input with
            | (true, d) when days.ContainsKey d ->
                days.[d] (AdventOfCode.Input.inputForDay d)
            | (_, d) when not (days.ContainsKey d) ->
                printfn "No solution for that day"
            | _ ->
                printfn "Type a number (or 'q' to quit)"
                waitForInput ()

    printfn "Choose a day to run"
    waitForInput ()
    0