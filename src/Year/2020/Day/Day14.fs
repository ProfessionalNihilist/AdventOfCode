module Year2020.Day14

open AdventOfCode
open System

type Instruction =
    | Mask of string
    | Memory of int64 * int64

let parseInstruction (i: string) =
    match i.[1] with
    | 'a' -> Mask (i.Substring(7))
    | 'e' ->
        let address = Int64.Parse(i.Substring(4, i.IndexOf(']') - 4))
        let value = Int64.Parse(i.Substring(i.IndexOf('=') + 2))
        Memory(address, value)
    | _ -> failwithf "unknown instruction %s" i

let dockingData (input: string) =
    let part1 = 
        let applyInstruction (mem, mask)  i =
            match i with
            | Mask m -> mem, m
            | Memory (a,v) ->
                let mutable value = v
                mask |> Seq.iteri (fun i x ->
                    match x with 
                    | '1' -> value <- value ||| (1L <<< 35 - i)
                    | '0' -> value <- value &&& ~~~(1L <<< 35 - i)
                    | _ -> ())
                mem |> Map.add a value, mask

        input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseInstruction
        |> (Seq.fold applyInstruction (Map.empty, String.Empty) >> fst)
        |> Map.toSeq |> Seq.sumBy snd

    let part2 =
        let applyInstruction (mem, (mask: string))  i =
            match i with
            | Mask m -> mem, m
            | Memory (address,value) ->
                let addresses, _ = 
                    let address, _ = 
                        mask |> Seq.fold (fun (s,i) x -> 
                            match x with 
                            | '1' -> s ||| (1L <<< 35 - i), i + 1
                            | 'X' -> s &&& ~~~(1L <<< 35 - i), i + 1
                            | _ -> s, i + 1) (address, 0)
                    mask |>Seq.fold (fun (si,i) x -> 
                        if x = 'X' then
                            si |> Seq.collect (fun a ->
                                [   a ||| (1L <<< 35 - i)
                                    a &&& ~~~(1L <<< 35 - i) ]), i + 1
                        else si, i + 1) (seq [address], 0)
                addresses |> Seq.fold (fun m a -> Map.add a value m) mem, mask

        input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map parseInstruction
            |> (Seq.fold applyInstruction (Map.empty, String.Empty) >> fst)
            |> Map.toSeq |> Seq.sumBy snd

    part1,part2