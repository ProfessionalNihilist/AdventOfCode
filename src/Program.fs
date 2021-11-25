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
    #if DEBUG
    let year = years |> Seq.map (fun x -> x.Key) |> Seq.max
    let day = years.[year] |> Seq.map (fun x -> x.Key) |> Seq.max
    years.[year].[day] (AdventOfCode.inputForDay year day)
    Environment.Exit(0)
    #endif

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

    match argv.Length with
    | 0 -> waitForInput ()
    | _ -> parser.Parse argv |> processInput
    0