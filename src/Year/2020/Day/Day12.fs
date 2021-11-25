module Year2020.Day12

open AdventOfCode
open System

type Heading = North | South | East | West
type MoveDir = N | S | E | W | F
type TurnDir = L | R

type Order =
    | Move of MoveDir * int
    | Turn of TurnDir * int

type State = { Facing: int; Ship: int * int; Waypoint: int * int }

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
        | _ -> failwithf "Invalid order %s" i

    let toHeading angle =
        match angle with
        | 0 -> N
        | 90 -> E
        | 180 -> S
        | 270 -> W
        | _ -> failwithf "non-cardinal angle %d" angle 

    let applyMove (x,y) heading distance =
        match heading with
        | N -> x, y + distance
        | S -> x, y - distance
        | E -> x + distance, y
        | W -> x - distance, y

    let rotate (ox,oy) dir =
        match dir, ox, oy with
        | L, x, y when x < 0 && y < 0 -> Math.Abs(x), y
        | R, x, y when x < 0 && y < 0 -> x, Math.Abs(y)
        | L, x, y when x < 0 -> x, -y
        | R, x, y when x < 0 -> -x, y
        | L, x, y when y < 0 -> x, -y
        | R, x, y when y < 0 -> x, -y
        | L, x, y -> -x, y
        | R, x, y -> x, -y

    let applyOrder (state: State) order =
        match order with
        | Turn (direction, angle) ->
            let op = if direction = L then (-) else (+)
            { state with 
                Facing = (360 + (op state.Facing angle)) % 360 }
        | Move (h, distance) ->
            let heading = if h = F then toHeading (state.Facing) else h
            { state with Ship = applyMove (state.Ship) heading distance }
            
    let finalPosition =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseOrder
        |> Seq.fold applyOrder { Facing = 90; Ship = 0,0; Waypoint = -10,1 }

    let (x,y) = finalPosition.Ship
    Answer.One (sprintf "manhattan distance: %d" (Math.Abs(x + y)))
