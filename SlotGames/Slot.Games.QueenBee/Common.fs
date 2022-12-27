namespace Slot.Games.QueenBee

open FSharpPlus
open Slot.Game.Common

module Common =
    type LeftRightLineResult<'a> = Core.LineResult<'a> * Core.LineResult<'a>
    type AllLineResult<'a> = list<LeftRightLineResult<'a>>

    let countLineTwice<'a when 'a: equality>
        (width: int)
        (isWild: 'a -> bool)
        (lineOfSymbol: list<'a>)
        : LeftRightLineResult<'a> =
        let leftResult = Core.countConsecutiveSymbols isWild lineOfSymbol

        match leftResult with
        | Some(_, c, _) when c = width -> (leftResult, None)
        | _ ->
            let rightResult = Core.countConsecutiveSymbols isWild (List.rev lineOfSymbol)
            (leftResult, rightResult)

    let countAllLineTwice<'a when 'a: equality>
        (width: int)
        (isWild: 'a -> bool)
        (linesOfSymbol: list<list<'a>>)
        : AllLineResult<'a> =
        linesOfSymbol |>> countLineTwice width isWild

    let countSymbol<'a> (test: 'a -> bool) =
        Array.sumBy (fun x -> if test x then 1 else 0)

    let scanScatter (snapshot: 'a[][]) (countScatter: 'a[] -> int) (countWild: 'a[] -> int) =
        let countRest = Array.map (fun reel -> (countScatter reel, countWild reel))

        let fold fc counts =
            let c, r =
                counts
                |> Array.fold (fun (c, r) (ts, tw) -> (c + ts + tw, r || (tw > 0))) (0, false)

            fc + c, r

        let countFirst (first: 'a[]) =
            let c = countScatter first
            if c = 0 then None else Some(c)

        let run fc =
            Array.tail snapshot
            |> countRest
            |> takeWhile (fun (s, w) -> s + w > 0)
            |> fold fc

        Array.tryHead snapshot >>= countFirst |>> run

    let countScatter<'a> (snapshot: 'a[][]) (isScatter: 'a -> bool) (isWild: 'a -> bool) =
        let cs = countSymbol isScatter
        let cw = countSymbol isWild
        let rl = scanScatter snapshot cs cw
        let rr = scanScatter (Array.rev snapshot) cs cw
        rl, rr
