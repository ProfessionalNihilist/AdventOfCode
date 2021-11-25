module Year2019.Day1

open AdventOfCode
open System

let theTryannyOfTheRocketEquation: Solution = fun rawInput ->
    let rec fuelForMass m =
        let fm = Math.Max(0, (m / 3) - 2)
        if fm = 0 then 0
        else fm + fuelForMass fm

    let fuel = rawInput.Split("\n")
                |> Seq.map Int32.TryParse
                |> Seq.filter fst
                |> Seq.map snd
                |> Seq.sumBy fuelForMass

    sprintf "Total fuel requirement: %d" fuel
        |> Answer.Two
