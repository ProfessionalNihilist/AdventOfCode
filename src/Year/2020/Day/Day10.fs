module Year2020.Day10

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

    int64 product, int64 validCombinations
