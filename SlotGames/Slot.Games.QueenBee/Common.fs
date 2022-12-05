module Slot.Games.QueenBee.Common

open FSharpPlus
open System.Collections.Generic
open System.Linq

type Reel = int []

module PayTable =
    let inline simpleLookup table count = Map.tryFind count table

    let nestedLookup table symbol count =
        monad {
            let! t = Map.tryFind symbol table
            return! simpleLookup t count
        }

module Level =
    let checkLevel (level: Reel list) (width: int) (height: int) =
        let rsl = List.length level

        if rsl <> width then
            invalidArg (nameof (level)) $"level should have {width} reels, but got {rsl}"
        else
            for reel in level do
                let rl = Array.length reel

                if rl < height then
                    invalidArg (nameof (level)) $"reel of level should NOT less than {height}, but got {rl}"

    let safePick reel = map (Array.get reel) 

    let safeRings (len: int) (start: int) (size: int) =
        seq { for i in start .. size + start - 1 -> i % len }

    let randomIdx (lens: seq<int>) (height: int) (random: int -> int) =
        [ for len in lens -> safeRings len (random len) height ]

    let randomSpin (reels: Reel list) (height: int) (random: int -> int) =
        let lens =
            seq { for l in reels -> Array.length l }

        let idx = randomIdx lens height random

        Seq.map2 (fun reel is -> (safePick reel is) |> Seq.toArray) reels idx
        |> Seq.toArray


module Symbol =
    let inline simpleLookup table count = Map.tryFind count table

    let nestedLookup table symbol count =
        monad {
            let! t = Map.tryFind symbol table
            return! simpleLookup t count
        }

module Line =
    let onePayLine (snapshot: int [] []) =
        Array.mapi (fun i j -> snapshot.[i].[j])

    let payLines (snapshot: int [] []) = Array.map (onePayLine snapshot)

    let consecutiveCount<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: IEnumerable<'a>) =
        let iter = lineOfSymbol.GetEnumerator()

        if iter.MoveNext() then
            let first = iter.Current

            if (isWild first) then
                None
            else
                let mutable consecutive = true
                let mutable count = 1

                while (iter.MoveNext() && consecutive) do
                    let e = iter.Current

                    if e = first || isWild e then
                        count <- count + 1
                    else
                        consecutive <- false

                Some(first, count)
        else
            None

    let countForth2Back<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: 'a []) =
        let l2r = lineOfSymbol.AsEnumerable()
        let leftResult = consecutiveCount isWild l2r
        let r2l = lineOfSymbol.Reverse()

        let rightResult =
            consecutiveCount isWild r2l

        (leftResult, rightResult) 