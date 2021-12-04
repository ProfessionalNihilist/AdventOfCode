module Year2021.Day3
open AdventOfCode
open System

let ``binary diagnostic``: Solution = fun (rawInput: string) ->
    let lines = asLines rawInput
    let counts = Array.zeroCreate 12
    lines |> Seq.iter (fun l ->
        l |> Seq.iteri (fun i c ->
            if c = '1' then counts.[i] <- counts.[i] + 1))

    let mutable gamma = 0u
    let threshold = (Seq.length lines) / 2
    counts |> Array.rev |> Array.iteri (fun i c ->
        if c > threshold then gamma <- gamma ||| (1u <<< i))

    let epsilon = ~~~(gamma ||| 4294963200u)
    let part1 = (gamma * epsilon)

    let findBits m =
        let keepBit (lines: string[]) p =
            let c1 = lines |> Seq.filter (fun l -> l.[p] = '1') |> Seq.length
            let c0 = Seq.length lines - c1
            match m with
            | true when c1 >= c0 -> '1'
            | false when c1 < c0 -> '1'
            | _ -> '0'

        let rec fold (lines: string[]) p =
            match lines with
            | [| x |] -> x
            | _ ->
                let c = keepBit lines p
                fold (Array.filter (fun l -> l.[p] = c) lines) (p + 1)

        let bits = fold lines 0
        Convert.ToInt32(bits, 2)

    let oxygen = findBits true
    let co2 = findBits false

    let part2 = oxygen * co2

    { Part1 = Ok (sprintf "%d" part1); Part2 = Ok (sprintf "%d" part2) }
