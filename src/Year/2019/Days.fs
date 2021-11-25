module Year2019.Days

open Year2019

let asMap = 
    [
        (1, Day1.theTryannyOfTheRocketEquation)
        (2, Day2.programAlarm)
        (3, Day3.crossedWires)
        (4, Day4.secureContainer)
        (5, Day5.sunnyWithAChanceOfAsteroids)
        (6, Day6.universalOrbitMap)
    ] |> Map.ofList

