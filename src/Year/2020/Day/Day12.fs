module Year2020.Day12

open AdventOfCode
open System

type Heading = North | South | East | West
type MoveDir = N | S | E | W | F
type TurnDir = L | R

type Order =
    | Move of MoveDir * int
    | Turn of TurnDir * int

type State = { Ship: int * int; Waypoint: int * int }

let rainRisk (input: string) =
    let parseOrder (i: string) =
        match i.[0], Int32.TryParse(i.Substring(1)) with
        | 'N', (true, v) -> Move (N, v)
        | 'S', (true, v) -> Move (S, v)
        | 'E', (true, v) -> Move (E, v)
        | 'W', (true, v) -> Move (W, v)
        | 'F', (true, v) -> Move (F, v)
        | 'L', (true, v) -> Turn (L, v)
        | 'R', (true, v) -> Turn (R, v)
        | _ -> failwith "unexpected direction"
 
    let applyOrder (state: State) order =
        match order with
        | Turn (direction, angle) ->
            let r (ox,oy) =
                match direction,ox,oy with
                | L, x, y -> -y, x
                | R, x, y -> y, -x
            let rotation = match angle / 90 with | 1 -> r | 2 -> r >> r | 3 -> r >> r >> r | _ -> failwith "too many rotations"
            { state with Waypoint = rotation state.Waypoint  }
        | Move (h, distance) ->
            match h with
            | F ->
                let (wx, wy) = state.Waypoint
                let (sx, sy) = state.Ship
                { state with Ship = sx + wx * distance, sy + wy * distance }
            | _ ->
                { state with Waypoint = match h, state.Waypoint with
                                        | N, (x, y) -> x, y + distance
                                        | S, (x, y) -> x, y - distance
                                        | E, (x, y) -> x + distance, y
                                        | W, (x, y) -> x - distance, y
                                        | _ -> failwith "illegal direction" }
            
    let finalPosition =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseOrder
        |> Seq.fold applyOrder { Ship = 0,0; Waypoint = 10,1 }

    let (x,y) = finalPosition.Ship
    Answer.Two (sprintf "manhattan distance: %d" (Math.Abs(x) + Math.Abs(y)))
