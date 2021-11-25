module Day3
open System

type Point = {X: int; Y: int }

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
        let mutable state = { X = 0; Y= 0 }

        [
            for p in wire.Split(",") do
                let (x, y) = offset p
                let i = Int32.Parse(p.[1..])
                for _ in 1 .. i do
                    state <- { state with
                                X = state.X + (x)
                                Y = state.Y + (y) }
                    yield (state.X, state.Y)
        ]

    let intersecions =
        Set.intersect
            (wireCoordinates w1 |> Set.ofSeq)
            (wireCoordinates w2 |> Set.ofSeq)

    let nearest = intersecions
                    |> Seq.map (fun (x,y) -> Math.Abs x + Math.Abs y)
                    |> Seq.filter (fun x -> x > 0)
                    |> Seq.min

    printfn "closest cross-over is %d" nearest