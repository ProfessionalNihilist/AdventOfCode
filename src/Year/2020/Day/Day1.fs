module Year2020.Day1

open AdventOfCode
open System

let solution: Solution = fun getInput ->
    let isExpense x y =
        match x + y with
        | 2020 -> x * y |> Some
        | _ -> None

    let splitOpts = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
    let expenses = (getInput ()).Split("\n", splitOpts)
                    |> Seq.map (Int32.Parse)

    let expense = expenses
                |> Seq.tryPick (fun x ->
                    expenses |> Seq.tryPick (isExpense x))

    match expense with
    | Some e -> printfn "Expense is %d" e
    | None -> printfn "No valid expense found?"

    ()

