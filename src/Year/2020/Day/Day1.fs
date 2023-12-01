module Year2020.Day1

open AdventOfCode
open System

let reportRepair: Solution = fun rawInput ->
    let splitOptions = StringSplitOptions.RemoveEmptyEntries
                        ||| StringSplitOptions.TrimEntries

    let expenses = rawInput.Split("\n", splitOptions)
                    |> Seq.map Int64.Parse

    let part1 = 
        let isExpense x y =
            match x + y with
            | 2020L -> x * y |> Some
            | _ -> None
        let expense = expenses
                    |> Seq.tryPick (fun x ->
                        expenses |> Seq.tryPick (isExpense x))
        Option.defaultValue 0L expense

    let part2 = 
        let isExpense x y z =
            match x + y + z with
            | 2020L -> x * y * z |> Some
            | _ -> None
        let expense = expenses
                    |> Seq.tryPick (fun x ->
                        expenses |> Seq.tryPick (fun y ->
                            expenses |> Seq.tryPick (isExpense x y)))
        Option.defaultValue 0L expense

    part1, part2
