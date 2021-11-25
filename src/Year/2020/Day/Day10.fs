module Year2020.Day10

open AdventOfCode
open System
open FSharp.Collections.ParallelSeq
open System.Collections.Generic


let comparer = 
    { new IEqualityComparer<HashSet<int>> with
            member this.Equals(l,r) = l.SetEquals(r)
            member this.GetHashCode s = s.GetHashCode()
    }

let memoize f =    
    let cache = new Dictionary<HashSet<int>, seq<HashSet<int>>>(comparer)
    (fun x ->
        match cache.TryGetValue x with
        | (true, item) -> item
        | _ -> 
            let item = f x
            cache.Add(x, item)
            item)

let adaptorArray (input: string) =
    let input = input.Split("\n", StringSplitOptions.RemoveEmptyEntries) |> Seq.map (Int32.Parse)
    let full = Seq.append input [0; Seq.max input + 3]
    let diffs = full |> (Seq.sort >> Seq.pairwise >> Seq.map (fun (l,r) -> r - l))
                |> (Seq.groupBy id >> Seq.map (fun (k,g) -> Seq.length g))
    let product = (Seq.head diffs) * (Seq.last diffs)
    
    let shortestChain = Seq.length full / 3
    let rec subsets = memoize (fun (s: HashSet<int>) ->
        seq {
            
            yield s
            if s.Count >= shortestChain then
                for e in s do
                    let sub = HashSet(s)
                    sub.Remove(e) |> ignore
                    yield! subsets (sub) })
    
    let validChain (source: seq<int>) =
        source 
        |> (Seq.pairwise >> Seq.map (fun (l,r) -> r - l))
        |> Seq.forall (fun d -> d >= 0 && d <= 3)

    let validCombinations = 
        (HashSet(full))
        |> subsets
        |> PSeq.map Seq.sort
        |> PSeq.filter validChain
        |> PSeq.length

    let rev = full |> Seq.sort |> Seq.rev |> List.ofSeq
    let rec chains s = 
        seq {
            yield! s
            match s with
            | h :: (h1 :: (h2 :: tail)) ->
                if h - h1 <= 3 then yield! chains tail
                if h - h2 <= 3 then yield! chains tail
            | h :: h1 :: [] -> if h - h1 >= 3 then yield! [h; h1]
            | [] -> ()
        }

    {   Part1 = Ok (sprintf "product: %d" product)
        Part2 = Ok (sprintf "%d valid combinations of adaptors" validCombinations) }

