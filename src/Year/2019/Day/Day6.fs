module Year2019.Day6

open AdventOfCode
open System
open FParsec

let ``universal orbit map`` input : int64 * int64 =
    let porbit = (manyChars asciiLetter) .>> pchar ')' .>>. (manyChars asciiLetter)
    let porbits = sepEndBy porbit newline

    let test = @"COM)B
    B)C
    C)D
    D)E
    E)F
    B)G
    G)H
    D)I
    E)J
    J)K
    K)L"

    let orbits = parseOrThrow porbits input
    
    
    0L,0L
