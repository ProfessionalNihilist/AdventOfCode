module AoC.Utilities
    open System.Net

    let authCookieContainer =
        let cookies = CookieContainer()
        let auth = Cookie(
                    "session",
                    "")
        auth.Domain <- "adventofcode.com"
        cookies.Add(auth)

        cookies