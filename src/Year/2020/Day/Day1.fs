module Year2020.Day1

open AdventOfCode
open System

let reportRepair: Solution = fun getInput ->
    let splitOptions = StringSplitOptions.RemoveEmptyEntries
                        ||| StringSplitOptions.TrimEntries

    let expenses = (getInput ()).Split("\n", splitOptions)
                    |> Seq.map Int32.Parse

    let part1 = 
        let isExpense x y =
            match x + y with
            | 2020 -> x * y |> Some
            | _ -> None
        let expense = expenses
                    |> Seq.tryPick (fun x ->
                        expenses |> Seq.tryPick (isExpense x))
        unwrap expense (sprintf "Expense is %d")

    let part2 = 
        let isExpense x y z =
            match x + y + z with
            | 2020 -> x * y * z |> Some
            | _ -> None
        let expense = expenses
                    |> Seq.tryPick (fun x ->
                        expenses |> Seq.tryPick (fun y ->
                            expenses |> Seq.tryPick (isExpense x y)))
        unwrap expense (sprintf "Expense is %d")

    { Part1 = part1; Part2 = part2 }
