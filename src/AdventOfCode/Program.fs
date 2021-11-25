// Learn more about F# at http://fsharp.org

open System
open System.Net
open FSharp.Data

[<EntryPoint>]
let main argv =
    let input = Http.RequestString( "https://adventofcode.com/2019/day/1/input",
                                    cookieContainer = AoC.Utilities.authCookieContainer)
    let fuel = input.Split("\n")
                |> Seq.map Int32.TryParse
                |> Seq.filter fst
                |> Seq.map snd
                |> Seq.sumBy (fun m -> (m / 3) - 2)

    printfn "Total fuel requirement: %d" fuel

    0 // return an integer exit code
