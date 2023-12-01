module Year2020.Day9

open AdventOfCode
open System

module Q =
    let sum (l,r) = List.sum l + List.sum r
    let length (l,r) = List.length l + List.length r
    let enqueue q x = (x :: fst q), snd q
    let dequeue = function
         | [], [] -> failwith "empty queue"
         | l, rh :: rs -> rh, (l, rs)
         | l, [] -> 
             let r = l |> List.rev
             r.Head, ([], r.Tail)

let encodingError (input: string) =
    let input = input.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Seq.map (Int64.Parse)
    let invalid xs =
        let x = xs |> Seq.last
        let isValid = xs  |> Seq.take ((xs |> Seq.length) - 1)
                          |> Seq.allPairs xs
                          |> Seq.exists (fun (l,r) -> l + r = x)
        if isValid then None else Some x
        
    let findWeakness nums windowSize =
        nums
        |> Seq.windowed (windowSize + 1)
        |> Seq.pick invalid
 
    let rec churn s q =
        let sum = Q.sum q
        match sum with
        | _ when Q.length q < 2 -> q, None
        | _ when sum = s ->  q, Some s
        | x when x > s ->
            let _, q = Q.dequeue q
            churn s q
        | _ -> q, None
        
    let findWindow sum source =
        source |> Seq.fold (fun (q,o) i ->
            match o with
            | None -> churn sum (Q.enqueue q i)
            | Some s -> q,o) (([],[]), None)
 
    let weakness = findWeakness input 25
    let window, _ = findWindow weakness input
    let joined = (snd window) @ (fst window)
    let weakSum = List.min joined + List.max joined
    
    weakness, weakSum
