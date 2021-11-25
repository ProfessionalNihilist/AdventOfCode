module Year2020.Days
open Year2020

let asMap = 
    [
        (1, Day1.reportRepair)
        (2, Day2.passwordPhilosophy)
    ] |> Map.ofList

