module AdventOfCode
    open System.IO
    open System.Net
    open FSharp.Data
    open System

    type Answer = {
        Part1: Result<string, string>
        Part2: Result<string, string>
    }
    with 
        static member NoAnswers = 
            { Part1 = Error "no answer"; Part2 = Error "no answer" }
        static member One answer =
            { Answer.NoAnswers with Part1 = Ok answer }
        static member Two answer =
            { Answer.NoAnswers with Part2 = Ok answer }

    let unwrap opt map =
        match opt with
        | None -> Error "no answer"
        | Some x -> Ok (map x)

    type Solution = string -> Answer

    let asLines (str: string) =
        str.Split("\n", StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries)

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
