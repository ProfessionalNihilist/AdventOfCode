module AdventOfCode
    open System.IO
    open System.Net
    open FSharp.Data
    open System

    type Answer = {
        Part1: string
        Part2: string
    }
    with 
        static member NoAnswers = 
            { Part1 = "no answer"; Part2 = "no answer" }
        static member One answer =
            { Answer.NoAnswers with Part1 = answer }
        static member Two answer =
            { Answer.NoAnswers with Part2 = answer }

    let unwrap opt map =
        match opt with
        | None -> "no answer"
        | Some x -> (map x)

    type Solution = string -> Answer

    let trimAndEmpty = StringSplitOptions.RemoveEmptyEntries
                        ||| StringSplitOptions.TrimEntries

    let asLines (str: string) =
        str.Split("\n", trimAndEmpty)

    let split (sep: string) (str: string) =
        str.Split(sep, trimAndEmpty)

    let authCookieContainer =
        let cookies = CookieContainer()
        let auth = Cookie(
                    "session",
                    "")
        auth.Domain <- "adventofcode.com"
        cookies.Add(auth)
        cookies

    let inputForDay year day =
        match (year, day) with
        | (2019, 4) -> "235741-706948"
        | _ ->
            Directory.CreateDirectory (sprintf "./inputs/%d" year) |> ignore
            let filename = sprintf "./inputs/%d/%d" year day
            match File.Exists filename with
            | false ->
                let uri = sprintf "https://adventofcode.com/%d/day/%d/input" year day
                let input = Http.RequestString(uri, cookieContainer = authCookieContainer)
                File.WriteAllText(filename, input)
                input
            | true ->
                File.ReadAllText filename

    module Array2D =
        let flatten (A:'a[,]) = A |> Seq.cast<'a>
