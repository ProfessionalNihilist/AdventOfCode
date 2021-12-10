module Year2021.Days
open Year2021

let asMap =
    [
        (1, Day1.sonarSweep)
        (2, Day2.``dive!``)
        (3, Day3.``binary diagnostic``)
        (4, Day4.``giant squid``)
        (5, Day5.``hydrothermal Venture``)
        (6, Day6.lanternfish)
        (7, Day7.``the treachery of whales``)
        //(10, Day10.``syntax scoring``)
    ] |> Map.ofList

