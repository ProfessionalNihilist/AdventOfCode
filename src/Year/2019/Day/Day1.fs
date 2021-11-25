module Year2019.Day1

open AdventOfCode
open System

let theTryannyOfTheRocketEquation: Solution = fun (getInput: unit -> string) ->
    let rec fuelForMass m =
        let fm = Math.Max(0, (m / 3) - 2)
        if fm = 0 then 0
        else fm + fuelForMass fm

    let fuel = getInput().Split("\n")
                |> Seq.map Int32.TryParse
                |> Seq.filter fst
                |> Seq.map snd
                |> Seq.sumBy fuelForMass

    printfn "Total fuel requirement: %d" fuel
