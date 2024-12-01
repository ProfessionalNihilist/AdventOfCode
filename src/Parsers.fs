[<AutoOpenAttribute>]
module ParserHelpers
open FParsec
    module Debugging =

        let BP (p: Parser<_,_>) stream =
            let r = p stream // set a breakpoint here
            r

        let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
            fun stream ->
                printfn "%A: Entering %s" stream.Position label
                let reply = p stream
                printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
                reply
