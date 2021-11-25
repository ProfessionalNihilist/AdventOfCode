module Year2019.Days

open Year2019

let asMap = 
    [
        (1, Day1.fuelCalculator)
        (2, Day2.runProgram)
        (3, Day3.run)
        (4, Day4.countPasswords)
        (5, Day5.runProgram)
    ] |> Map.ofList

