module Year2020.Day7

open AdventOfCode
open System
open FParsec

type Rule = string * int
type Contains =
    | Nothing
    | Rules of (string * int) list

let handyHaversacks (input: string) =
    let bag = manyCharsTill (asciiLetter <|> pchar ' ') (skipString " bag" .>> (optional (pchar 's')))
    let label = bag .>> (pstring " contain ")
    let none = pstring "no other bags." |>> (fun _ -> Nothing)
    let rule = (digit .>>. bag) |>> (fun (d,l) -> l.Trim(), Int32.Parse(d.ToString()))
    let rules = sepBy rule (pstring ", ") |>> (fun r -> Rules (r)) .>> pchar '.'
    let rulesForBags = sepEndBy (label .>>. (none <|> rules)) (skipNewline <|> eof)

    match run rulesForBags input with
    | Failure (error,_,_) -> failwith error
    | Success (result, _, _) ->
        let bagsToRules = result |> Map.ofList
        let rec canContain (b: string) rule =
            match rule with
            | Nothing -> false
            | Rules rules ->
                rules |> Seq.exists (fun (l,_) ->
                                match l.Contains b with
                                | true -> true
                                | false ->
                                    match Map.containsKey l bagsToRules with
                                    | false -> false
                                    | true -> canContain b (bagsToRules.[l]))
             
        let count = 
            bagsToRules
            |> Map.filter (fun _ r -> canContain "shiny gold" r)
            |> Seq.length


        let rec countContainers rules =
            match rules with
            | Nothing -> 1
            | Rules rs ->
                1 + (rs |> Seq.sumBy (fun (r,c) -> c * countContainers (bagsToRules.[r])))
                
                // -1 for the gold bag that doesn't contain itself
        bagsToRules.["shiny gold"] |> countContainers
            |> printfn "%d" 

        int64 count,0L
