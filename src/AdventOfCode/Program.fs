// Learn more about F# at http://fsharp.org

open System
open System.Net
open FSharp.Data

[<EntryPoint>]
let main argv =
    let input = Http.RequestString( "https://adventofcode.com/2019/day/1/input",
                                    cookieContainer = AoC.Utilities.authCookieContainer)

    let rec fuelForMass m =
        let fm = Math.Max(0, (m / 3) - 2)
        if fm = 0 then 0
        else fm + fuelForMass fm

    let fuel = input.Split("\n")
                |> Seq.map Int32.TryParse
                |> Seq.filter fst
                |> Seq.map snd
                |> Seq.sumBy fuelForMass

    printfn "Total fuel requirement: %d" fuel

    0 // return an integer exit code
