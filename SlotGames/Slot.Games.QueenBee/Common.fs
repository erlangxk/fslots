module Slot.Games.QueenBee.Common

open FSharpPlus
open System.Collections.Generic
open System.Linq

type Reel<'a> = 'a[]

module PayTable =
    let inline simpleLookup table count = Map.tryFind count table

    let nestedLookup table symbol count =
        monad {
            let! t = Map.tryFind symbol table
            return! simpleLookup t count
        }

module Level =
    let checkLevel<'a> (level: Reel<'a> list) (width: int) (height: int) =
        let rsl = List.length level

        if rsl <> width then
            invalidArg (nameof (level)) $"level should have {width} reels, but got {rsl}"
        else
            for reel in level do
                let rl = Array.length reel

                if rl < height then
                    invalidArg (nameof (level)) $"reel of level should NOT less than {height}, but got {rl}"

    let safePick<'a> (reel:Reel<'a>)(idx: seq<int>) = Seq.map (Array.get reel) idx

    let safeRings (len: int) (start: int) (size: int) =
        seq { for i in start .. size + start - 1 -> i % len }

    let fakeRandomSeq (numbers:seq<int>) =
         let state = numbers.GetEnumerator()
         let rec f (i:int) =
             if state.MoveNext() then
                 state.Current
             else
                 state.Reset()
                 f i
         f
    
    let randomIdx (lens: seq<int>)(height: int)(random: int -> int) =
        [ for len in lens -> safeRings len (random len) height ]

    let randomSpin<'a> (height: int)(reels: Reel<'a> list)(random: int -> int) =
        let lens =
            seq { for l in reels -> Array.length l }

        let idx = randomIdx lens height random

        Seq.map2 (fun reel is -> (safePick reel is) |> Seq.toArray) reels idx
        |> Seq.toArray
 

type LineResult<'a> = option<'a * int * bool>
module Line =
    let onePayLine<'a>(snapshot: 'a[][]) =
        Array.mapi( fun i j -> snapshot.[i].[j])
    
    let payLines<'a> (lines: int[][]) (snapshot : 'a[][]) = 
        lines |> Array.map (onePayLine snapshot)
    
    let countLineOnce<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: seq<'a>) : LineResult<'a>=
        let iter = lineOfSymbol.GetEnumerator()

        if iter.MoveNext() then
            let first = iter.Current

            if (isWild first) then
                None
            else
                let mutable consecutive = true
                let mutable count = 1
                let mutable replace  = false
                while (iter.MoveNext() && consecutive) do
                    let e = iter.Current
                    let w = isWild e
                    replace <- replace || w
                    if e = first || w then
                        count <- count + 1
                    else
                        consecutive <- false

                Some(first, count, replace)
        else
            None

    let countLineTwice<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: 'a []) =
        let l2r = lineOfSymbol.AsEnumerable()
        let leftResult = countLineOnce isWild l2r
        
        let r2l = lineOfSymbol.Reverse()
        let rightResult = countLineOnce isWild r2l

        (leftResult, rightResult)
        
    let countAllLineTwice<'a when 'a: equality>(isWild: 'a->bool)(linesOfSymbol: 'a[][]) =
       linesOfSymbol |> Array.map (countLineTwice isWild)
    
    let countSymbol<'a> (test:'a->bool) =
        Seq.sumBy (fun x -> if test x then 1 else 0)
        
    let scanScatter<'a> (snapshot: seq<'a[]>) (countScatter: seq<'a> -> int) (countWild: seq<'a> -> int) =
        let iter = snapshot.GetEnumerator()
        if iter.MoveNext() then 
            let first = countScatter iter.Current
            if first > 0 then
                let mutable consecutive = true
                let mutable total = first
                let mutable replace = false
                while (iter.MoveNext() && consecutive) do
                    let reel = iter.Current
                    let si = countScatter reel
                    let wi = countWild reel
                    let ti = si + wi
                    if ti>0 then
                         total <- total + ti
                    else
                        consecutive <- false
                    if wi > 0 then replace <- true
                Some(total, replace)
            else
                None
        else None

    let countScatter<'a> (snapshot: 'a[][]) (isScatter:'a->bool) (isWild: 'a->bool) =
            let cs = countSymbol isScatter
            let cw = countSymbol isWild
            let el2r = snapshot.AsEnumerable()
            let rl = scanScatter el2r cs cw
            let er2l = snapshot.Reverse()
            let rr = scanScatter er2l cs cw
            rl,rr