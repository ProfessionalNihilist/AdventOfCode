module AdventOfCode
    open System.IO
    open System.Net
    open FSharp.Data
    open System
    open FParsec

    let parseOrThrow (p:Parser<'a, unit>) (i: string) =
        match runParserOnString p () "" i with
        | Success (r,_,_) -> r
        | Failure (m,e,_) -> failwithf "%s %A" m e

    let parseOrThrowS (p:Parser<'a, 'u>) (s: 'u) (i: string) =
        match runParserOnString p s "" i with
        | Success (r,s,_) -> r,s
        | Failure (m,e,_) -> failwithf "%s %A" m e

    let unwrap opt map =
        match opt with
        | None -> 0L
        | Some x -> (map x)

    type Solution = string -> (int64 * int64)

    let trimAndEmpty = StringSplitOptions.RemoveEmptyEntries
                        ||| StringSplitOptions.TrimEntries

    let asLines (str: string) =
        str.Split("\n", trimAndEmpty)

    let split (sep: string) (str: string) =
        str.Split(sep, trimAndEmpty)

    let authCookieContainer () =
        let cookies = CookieContainer()
        let token = 
            match Environment.GetEnvironmentVariable("ADVENTOFCODE") with
            | x when String.IsNullOrWhiteSpace x ->  failwith "session token environment variable not set"
            | x -> x

        let auth = Cookie("session", token)
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
                let cookies = authCookieContainer ()
                let input = Http.RequestString(uri, cookieContainer = cookies)
                File.WriteAllText(filename, input)
                input
            | true ->
                File.ReadAllText filename

    module String =
        let toLines (str: string) = str.Split("\n", trimAndEmpty)

    module Array2D =
        open System.Numerics
        open System.Text
        open System.Collections.Generic

        let ofString input =
            let grid = input |> asLines |> Array.map (fun s -> s.ToCharArray ())
            Array2D.init (grid.Length) (grid.[0].Length) (fun x y -> grid.[x].[y])

        let equalsVec (left: 'a [,]) (right: 'a [,]) =
            let areEqual i =
                let l = left.[i,0..]
                let r = right.[i,0..]

                let last = l.Length - (l.Length % Vector<'a>.Count)
                let mutable c = 0
                let mutable equals = true

                while equals && c < last do
                    equals <- Vector(l, c).Equals(Vector(r, c))
                    c <- c + Vector<'a>.Count

                while equals && c < l.Length do
                    equals <- l.[c] = r.[c]
                    c <- c + 1
                equals

            if left.Length = right.Length && Array2D.length1 left = Array2D.length1 right then
                let indices = Array.init (Array2D.length1 left) id
                indices |> Array.forall areEqual
            else false

        let countBy (f: 'a -> bool) (s: 'a [,]) =
            let mutable count = 0       
            for x in 0 .. (Array2D.length1 s) - 1 do
                for y in 0 .. (Array2D.length2 s) - 1 do
                    if f (s.[x,y]) then count <- count + 1 
            count
        
        let flatten (A:'a[,]) = A |> Seq.cast<'a>

        let toString (s: 'a [,]) =
            let b = StringBuilder ()
            for x in 0 .. (Array2D.length1 s) - 1 do
                let bx = StringBuilder ()
                for y in 0 .. (Array2D.length2 s) - 1 do
                    bx.Append (s.[x,y]) |> ignore
                b.AppendLine (bx.ToString ()) |> ignore
            b.ToString ()

        let tryPicki (f: int * int -> 'a -> 'b option) (s: 'a [,])  =
            let mutable result = None
            let mutable found = false

            for x in 0 .. (Array2D.length1 s) - 1 do
                for y in 0 .. (Array2D.length2 s) - 1 do
                    // todo: end early
                    if found = false then
                        result <- f (x,y) (s[y,x])
                        found <- result <> None
            result

        let picki (f: int * int -> 'a -> 'b option) (s: 'a [,])  =
            match tryPicki f s with
            | Some item -> item
            | None -> raise (KeyNotFoundException ())

        let max (s: 'a [,]) =
            let mutable m = s[0,0]
            Array2D.iter (fun x -> if x > m then m <- x) s
            m

        let choosei (f: int * int -> 'a -> 'b option) (s: 'a [,]) =
            seq {
                for x in 0 .. (Array2D.length1 s) - 1 do
                    for y in 0 .. (Array2D.length2 s) - 1 do
                        match f (x,y) s[y,x] with | Some a -> yield a | None -> ()
            } |> Array.ofSeq

        let bounds (s: 'a [,]) = (Array2D.length1 s - 1, Array2D.length2 s - 1)