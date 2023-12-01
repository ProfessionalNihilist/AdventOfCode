module Year2020.Day5

open AdventOfCode

type Ticket = 
    { Row: int
      Column: int
    }
        member this.SeatId = this.Row * 8 + this.Column
        static member New (r,c) =
            { Row = r; Column = c}

let binaryBoarding (input: string) =
    let parseTicket ticket =
        let mutable rMax = 127
        let mutable rMin = 0
        let mutable cMax = 7
        let mutable cMin = 0
        for c in ticket do
            match c with
            | 'B' -> rMin <- (rMax - rMin + 1) / 2 + rMin
            | 'F' -> rMax <- rMin + ((rMax - rMin) / 2)
            | 'R' -> cMin <- (cMax - cMin + 1) / 2 + cMin
            | 'L' -> cMax <- cMin + ((cMax - cMin) / 2)
            | _ -> failwith "impossible row"
        rMin,cMin

    let findAdjacentSeats (t: Ticket[]) =
        t.[0].SeatId + 1 = t.[1].SeatId
            && t.[1].SeatId + 1 = t.[2].SeatId
            |> not

    let findSeat (l: Ticket, r: Ticket) =
        match r.SeatId - l.SeatId with
        | 2 -> l.SeatId + 1 |> Some
        | _ -> None

    let tickets = input.Split('\n') |> Seq.map (parseTicket >> Ticket.New)
    let seatId = 
        tickets
        |> Seq.sortBy (fun x -> x.SeatId)
        |> Seq.windowed 3
        |> Seq.filter findAdjacentSeats
        |> Seq.head
        |> Seq.pairwise
        |> Seq.pick findSeat

    let part1 = (tickets |> Seq.map (fun x -> x.SeatId) |> Seq.max )
    
    int64 part1, int64 seatId
        
