module Year2020.Day4

open AdventOfCode
open System
open FParsec

type Hgt = Cm | In

let passportProcessing (input: string) =
    let keyValue = manyCharsTill asciiLetter (pchar ':')
                    .>>. manyCharsTill (satisfy (Char.IsWhiteSpace >> not))
                                       (skipNewline <|> eof <|> skipChar ' ')
    let records = sepEndBy1 (many1 keyValue) (skipNewline <|> eof)
    let pHeight = many1Satisfy isDigit .>>. (pstring "cm" >>% Cm <|> (pstring "in" >>% In))
    let pHair = manyMinMaxSatisfy2 7 7 ((=) '#') isHex .>> eof

    let isNumberBetween lower upper (s: string) =
        match Int32.TryParse s with
        | (true, n) when n >= lower && n <= upper -> true
        | _ -> false
        
    let isValidHeight i =
        match run pHeight i with
        | Success((m,h),_,_) ->
            match (h, Int32.TryParse m) with
            | (Cm, (true, u)) when u >= 150 && u <= 193 -> true
            | (In, (true, u)) when u >= 59 && u <= 76 -> true
            | _ -> false
        | _ -> false

    let isValidHairColour i =
        match run pHair i with
        | Success(_) -> true
        | _ -> false

    let isValidEyeColour ecl =
        ["amb";"blu";"brn";"gry";"grn";"hzl";"oth"] |> List.exists ((=) ecl)
        
    let isValidPassportNumber i =
        match run (parray 9 digit .>> eof) i with
        | Success(_) -> true
        | _ -> false

    let hasKeys =
        let keys = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] |> Set
        Seq.map fst >> Set >> Set.isSubset keys

    let keysValid record =
        let r = Map.ofSeq record
        [
            "byr", isNumberBetween 1920 2002
            "iyr", isNumberBetween 2010 2020
            "eyr", isNumberBetween 2020 2030
            "hgt", isValidHeight
            "hcl", isValidHairColour
            "ecl", isValidEyeColour
            "pid", isValidPassportNumber
        ] |> List.forall (fun (k,validator) ->
                        match Map.tryFind k r with
                        | Some v -> validator v
                        | _ -> false)
        
    match run records input with
    | Success (result,_,_) -> 
        let hasValidKeys = result |> Seq.filter hasKeys |> Seq.length |> int64
        let keyValuesAreValid = result |> Seq.filter hasKeys |> Seq.filter keysValid |> Seq.length |> int64
        hasValidKeys, keyValuesAreValid
    | Failure (error,_,_) ->
        System.Diagnostics.Debugger.Break()
        failwith error
