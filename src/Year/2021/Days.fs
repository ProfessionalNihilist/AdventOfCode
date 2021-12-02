module Year2021.Days
open Year2021

let asMap =
    [
        (1, Day1.sonarSweep)
        (2, Day2.``dive!``)
    ] |> Map.ofList

