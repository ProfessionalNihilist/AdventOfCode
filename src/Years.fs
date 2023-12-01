module Years
    let mutable known = Map<int * int, string -> int64 * int64>([])

    let add year day solution =
        known <- known.Add( (year,day), solution )
        ()

    let get year day =
        known.TryFind (year,day)