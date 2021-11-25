module Year2020.Days
open Year2020

let asMap = 
    [
        (1, Day1.reportRepair)
        (2, Day2.passwordPhilosophy)
        (3, Day3.tobogganTrajectory)
        (4, Day4.passportProcessing)
        (5, Day5.binaryBoarding)
        (6, Day6.customCustoms)
        (7, Day7.handyHaversacks)
        (8, Day8.handheldHalting)
        (9, Day9.encodingError)
        (10, Day10.adaptorArray)
        (11, Day11.seatingSystem)
        (12, Day12.rainRisk)
    ] |> Map.ofList

