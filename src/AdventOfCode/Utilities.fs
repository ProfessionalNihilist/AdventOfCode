module AdventOfCode.Input
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
        let uri = sprintf "https://adventofcode.com/2019/day/%d/input" day
        fun () -> Http.RequestString(uri, cookieContainer = authCookieContainer)
