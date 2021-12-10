module Year2020.Day6

open AdventOfCode
open System

let customCustoms (input: string) =
    let forms = input.Split("\n\n")
    let countQuestions (group: string seq) =
        let answered = group |> Seq.concat |> Seq.distinct |> Seq.filter (Char.IsLetter)
        answered |> Seq.filter (fun c -> (group |> Seq.forall (Seq.contains c)))
        |> Seq.length

    { Part1 = sprintf "sum is %d" (
                forms |> Seq.sumBy
                    (Seq.filter Char.IsLetter >> Seq.distinct >> Seq.length))
      Part2 = sprintf "sum is %d" (
                forms
                |> Seq.map (fun x -> x.Split("\n", StringSplitOptions.RemoveEmptyEntries))
                |> Seq.sumBy countQuestions) }
