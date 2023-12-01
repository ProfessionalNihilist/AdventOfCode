module Year2020.Day11

open AdventOfCode
open System

let seatingSystem (input: string) =
    let length = input |> Seq.takeWhile (Char.IsWhiteSpace >> not) |> Seq.length
    let chars = input |> Seq.filter (Char.IsWhiteSpace >> not)
    let width = (Seq.length chars / length) 
    let grid = Array2D.zeroCreate length width
    
    let floor = 1uy
    let empty = 2uy
    let occupied = 3uy

    let directions = [| -1,0; -1,-1; 0,-1; 1,-1; 1,0; 1,1; 0,1; -1,1 |]
    let toByte = function | 'L' -> empty | '#' -> occupied | _ -> floor
    chars |> Seq.iteri (fun i x -> grid.[i % length, i / length] <- toByte x)

    let neighbourCoords x y =
        directions
        |> Array.map (fun (dx,dy) -> x + dx, y + dy)
        |> Array.filter (fun (nx,ny) -> nx >= 0 && ny >= 0 && nx < length && ny < width)

    let neighbourVecs x y =
        seq {
            for (dx,dy) in directions do
                yield Seq.unfold (fun i -> 
                    let nx = x + dx * i
                    let ny = y + dy * i
                    match nx, ny with
                    | _ when nx < 0 || ny < 0 -> None
                    | _ when nx < length && ny < width -> Some ((nx,ny), i + 1)
                    | _ -> None) 1
        }    

    let applyRule (source: byte [,]) x y c =
        match c with
        | _ when c = empty -> 
            let apply = 
                neighbourVecs x y
                |> Seq.choose (Seq.tryPick (fun (x,y) ->
                    if source.[x,y] <> floor then Some source.[x,y] else None))
                |> Seq.forall ((<>) occupied)
            if apply then occupied else c
        | _ when c = occupied ->
            let apply =
                neighbourVecs x y
                |> Seq.choose (Seq.tryPick (fun (x,y) ->
                    if source.[x,y] <> floor then Some source.[x,y] else None))
                |> Seq.filter ((=) occupied)
                |> Seq.length >= 5
            if apply then empty else c
        | _ -> c

    let applyRules arr = arr |> Array2D.mapi (applyRule arr)
    let mutable current = grid
    let mutable next = applyRules grid
    while Array2D.equalsVec current next |> not  do
        current <- next
        next <- applyRules current

    let occupied = next |> Array2D.countBy (fun x -> x = 3uy)

    0L, int64 occupied
