module Year2020.Day13

open AdventOfCode
open System
open FSharp.Collections.ParallelSeq

let shuttleSearch (input: string) =
    let input = input.Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let part1 =
        let earliestDeparture = Int32.Parse (input.[0])
        let busTimes = input.[1].Split(',') |> Seq.choose (fun s ->
            match Int32.TryParse s with false, _ -> None | true, i -> Some i )
        let catcheable = busTimes |> Seq.map (fun x -> 
            Seq.initInfinite ((*) x) |> Seq.pick (fun y -> 
                if y >= earliestDeparture then Some (x,y) else None))
        let (busId,takenAt) =  catcheable |> Seq.minBy snd
        busId * (takenAt - earliestDeparture)

    let part2 (input: string) =
        let busTimes = input.Split(',') |> Seq.mapi (fun i x -> 
                        match Int64.TryParse x with
                        | true, t -> Some (t, int64 i)
                        | false, _ -> None)
                        |> Seq.choose id
                        |> Seq.toArray

        let valid t = if busTimes |> Seq.forall (fun (b,o) -> (t - o) % b = 0L) then Some t else None
        let largest = 81229L // busTimes |> Seq.map fst |> Seq.max
        let floor = (100000000000000L / largest + 1L) * largest
        let first = busTimes |> Seq.head |> fst
        let second, offset = busTimes |> Seq.skip 1 |> Seq.head
        //let initial = Seq.initInfinite id |> Seq.find (fun n -> if int64 n % first = 0L && (int64 n + offset) % second = 0L then true else false) |> int64
        //let step, ``step ?`` =
        //        busTimes |> Seq.skip 2
        //        |> Seq.fold(fun(prev, s)(prime, o)->
        //            let nums = Seq.initInfinite (fun n -> (s + (prev * int64 n)))
        //            let next = nums |> Seq.find (fun n-> if (n + o) % prime = 0L then true else false)
        //            next, next * s) (first * second, initial)
        PSeq.init Int32.MaxValue (fun x -> int64 x * 29L + floor) |> PSeq.pick valid

    //[
    //"7,13,x,x,59,x,31,19", 1068781L
    //"17,x,13,19", 3417L
    //"67,7,59,61", 754018L
    //"67,x,7,59,61", 779210L
    //"67,7,x,59,61", 1261476L
    //"1789,37,47,1889", 1202161486L ]
    //    |> List.iter (fun (i,e) -> if part2 i <> e then failwithf "failed on %s" i)
        
    { Part1 = Ok (sprintf "%d" part1); Part2 = Ok (sprintf "%d" (part2 input.[1])) }

