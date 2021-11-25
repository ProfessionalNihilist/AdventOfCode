module Year2019.Day4

open AdventOfCode

let secureContainer (getInput: unit -> string) =
    let input = getInput()
    let min = input.Split("-").[0] |> int32
    let max = input.Split("-").[1] |> int32

    let validPasswords = seq {
        for i in min .. max do
            let parts = [
                i / 100000
                (i / 10000) % 10
                (i / 1000) % 100 % 10
                (i / 100) % 1000 % 100 % 10
                (i / 10) % 10000 % 1000 % 100 % 10
                i % 100000 % 10000 % 1000 % 100 % 10 ]

            let allIncrease =
                parts
                |> List.pairwise
                |> List.forall (fun (l,r) -> l <= r)

            let packDuplicates xs =
                let collect x = function
                | (y::xs)::xss when x = y -> (x::y::xs)::xss
                | xss -> [x]::xss
                List.foldBack collect xs []

            let valid =
                packDuplicates parts
                |> List.exists (fun l -> l |> List.length = 2)

            if allIncrease && valid then
                yield i
    }

    sprintf "Found %d possible passwords" (validPasswords |> Seq.length)
        |> Answer.Two

