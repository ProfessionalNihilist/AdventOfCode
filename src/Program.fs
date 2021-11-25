open System
open Argu

let years = [
        (2019, Year2019.Days.asMap)
        (2020, Year2020.Days.asMap)
            ] |> Map.ofList

type Arguments =
    | [<Mandatory>] Year of year:int
    | [<Mandatory>] Day of day:int
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Year _ -> "specify a year"
            | Day _ -> "specify a day"

[<EntryPoint>]
let main argv =
    let getInput =  2019
    let parser = ArgumentParser.Create<Arguments>(programName = "AdventOfCode.exe")

    let processInput (i: ParseResults<Arguments>) =
        
        let year = i.GetResult Year
        let day = i.GetResult Day
        years.[year].[day] (AdventOfCode.inputForDay year day)

    let rec waitForInput () =
        printfn "Choose a year / day to run"
        let input = Console.ReadLine()

        match input with
        | "q" -> ()
        | "h" -> Console.WriteLine(parser.PrintUsage ())
        | _ -> 
            processInput <| parser.Parse([|input|]) 
            waitForInput ()
            //match Int32.TryParse input with
            //| (true, d) when days.ContainsKey d ->
            //    days.[d] (getInput d)

            //| (_, d) when not (days.ContainsKey d) ->
            //    printfn "No solution for that day"
            //| _ ->
            //    printfn "Type a number (or 'q' to quit)"

    match argv.Length with
    | 0 -> waitForInput ()
    | _ -> parser.Parse argv |> processInput
    0