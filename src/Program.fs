open System
open Argu
open AdventOfCode

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
    Year2019.Days.register ()
    Year2020.Days.register ()
    Year2021.Days.register ()
    #if DEBUG

    let (year,day), solution = Years.known |> Map.maxKeyValue
    let (part1, part2) = solution (inputForDay year day) 
    printfn "Year %d Day %d\n%d\%d" year day part1 part2
    Environment.Exit(0)
    
    #endif

    let parser = ArgumentParser.Create<Arguments>(programName = "AdventOfCode.exe")
    let processInput input =
        let arguments = parser.Parse input
        let year = arguments.GetResult Year
        let day = arguments.GetResult Day
        match Years.get year day with
        | Some s -> 
            let (part1, part2) = s (inputForDay year day)
            printfn "Part 1: %d\nPart 2: %d" part1 part2
        | _ -> failwithf "Failed to find year/day %d/%d" year day

    let rec waitForInput () =
        printfn "Choose a year / day to run"
        let input = Console.ReadLine()
        match input with
        | "q" -> ()
        | "h" | "?" -> Console.WriteLine(parser.PrintUsage ())
        | _ -> 
            processInput [| input |]
            waitForInput ()

    match argv.Length with
    | 0 -> waitForInput ()
    | _ -> processInput argv
    0