module Day3
open System

type Point = {X: int; Y: int; Step: int }

let run (getInput: unit -> string) =
    let wires = getInput().Split("\n")
    let w1 = wires.[0]
    let w2 = wires.[1]

    let offset (path: string) =
        match path.[0] with
        | 'D' -> (0, -1)
        | 'U' -> (0, 1)
        | 'L' -> (-1, 0)
        | 'R' -> (1, 0)
        | _ -> (0, 0)

    let wireCoordinates (wire: string) =
        let mutable state = { X = 0; Y= 0; Step = 0 }

        [
            for p in wire.Split(",") do
                let (x, y) = offset p
                let i = Int32.Parse(p.[1..])
                for _ in 1 .. i do
                    state <- { state with
                                X = state.X + (x)
                                Y = state.Y + (y)
                                Step = state.Step + 1}
                    yield ((state.X, state.Y), state.Step)
        ]

    let w1c = wireCoordinates w1
    let w2c = wireCoordinates w2

    let intersections =
        Set.intersect
            (w1c |> Seq.map fst |> Set.ofSeq)
            (w2c |> Seq.map fst |> Set.ofSeq)

    let w1cm = Map.ofSeq w1c
    let w2cm = Map.ofSeq w2c

    let nearest = intersections
                    |> Seq.map (fun x -> w1cm.[x] + w2cm.[x])
                    |> Seq.min

    printfn "shortest length cross-over is %A" nearest