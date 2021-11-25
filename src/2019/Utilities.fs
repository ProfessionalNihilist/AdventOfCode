module AdventOfCode.Input
    open System.IO
    open System.Net
    open FSharp.Data

    let authCookieContainer =
        let cookies = CookieContainer()
        let auth = Cookie(
                    "session",
                    "")
        auth.Domain <- "adventofcode.com"
        cookies.Add(auth)
        cookies

    let inputForDay day =
        match day with
        | 4 -> fun () -> "235741-706948"
        | _ ->
            let filename = sprintf "./inputs/%d" day
            match File.Exists filename with
            | false ->
                let uri = sprintf "https://adventofcode.com/2019/day/%d/input" day
                let input = Http.RequestString(uri, cookieContainer = authCookieContainer)
                File.WriteAllText(filename, input)
                fun () -> input
            | true ->
                fun () -> File.ReadAllText filename
