module AdventOfCode.Input
    open System.IO
    open System.Net
    open FSharp.Data

    let authCookieContainer =
        let cookies = CookieContainer()
        let auth = Cookie(
                    "session",
                    "53616c7465645f5fa63391ac93bd62b7c63de0defa64ec18e07392959b69f03fe28537930c4fe2c922a0e75e25ad0ac0")
        auth.Domain <- "adventofcode.com"
        cookies.Add(auth)
        cookies

    let inputForDay day =
        let filename = sprintf "./inputs/%d" day
        match File.Exists filename with
        | false ->
            let uri = sprintf "https://adventofcode.com/2019/day/%d/input" day
            let input = Http.RequestString(uri, cookieContainer = authCookieContainer)
            File.WriteAllText(filename, input)
            fun () -> input
        | true ->
            fun () -> File.ReadAllText filename
