module AdventOfCode
    open System.IO
    open System.Net
    open FSharp.Data

    type Solution = (unit -> string) -> unit

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
        | (2019, 4) -> fun () -> "235741-706948"
        | _ ->
            let filename = sprintf "./inputs/%d" day
            match File.Exists filename with
            | false ->
                let uri = sprintf "https://adventofcode.com/%d/day/%d/input" year day
                let input = Http.RequestString(uri, cookieContainer = authCookieContainer)
                File.WriteAllText(filename, input)
                fun () -> input
            | true ->
                fun () -> File.ReadAllText filename
