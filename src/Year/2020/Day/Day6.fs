module Year2020.Day6

open AdventOfCode
open System

let customCustoms (input: string) =
    let forms = input.Split("\n\n")
    let countQuestions (group: string seq) =
        let answered = group |> Seq.concat |> Seq.distinct |> Seq.filter (Char.IsLetter)
        answered |> Seq.filter (fun c -> (group |> Seq.forall (Seq.contains c)))
        |> Seq.length

    let part1 = sprintf "sum is %d" (
                forms |> Seq.sumBy
                    (Seq.filter Char.IsLetter >> Seq.distinct >> Seq.length))
    let part2 = sprintf "sum is %d" (
                forms
                |> Seq.map (fun x -> x.Split("\n", StringSplitOptions.RemoveEmptyEntries))
                |> Seq.sumBy countQuestions)

    int64 part1, int64 part2
