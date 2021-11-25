module Year2020.Day2

open System

let passwordPhilosophy (getInput: unit -> string) =
    let matchesRule (rule: string) =
        let mutable i = 0

        let reader = seq {
            while i < (rule.Length) do
                yield rule.[i]
                i <- i + 1
        }

        let readInt = Seq.takeWhile (Char.IsDigit)
                        >> String.Concat
                        >> Int32.Parse

        let min = readInt reader
        let max = reader |> Seq.skip 1 |> readInt
        let character = reader |> Seq.skip 1 |> Seq.head
        let password = reader |> Seq.skip 2 |> String.Concat

        let chars = password |> Seq.where ((=) character) |> Seq.length
        chars >= min && chars <= max

    let valid = (getInput ()).Split("\n", StringSplitOptions.RemoveEmptyEntries)
                |> Seq.where matchesRule
                |> Seq.length

    printfn "%d valid passwords" valid


