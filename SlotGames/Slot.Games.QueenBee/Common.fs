module Slot.Games.QueenBee.Common

open FSharpPlus
open System.Linq

type Reel<'a> = 'a[]

module PayTable =
    let simpleLookup = Map.tryFind
    let nestedLookup symbol count = simpleLookup symbol >=> simpleLookup count
        
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

    let safePick<'a> (reel:Reel<'a>)(idx: seq<int>) = idx |>> Array.get reel 

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

    let shoot(reels: Reel<'a> list)(reelIndex: seq<seq<int>>) =
        let oneReel reel is =  (safePick reel is) |> Seq.toArray
        Seq.map2 oneReel reels reelIndex |> Seq.toArray
    
    let randomSpin<'a> (height: int)(reels: Reel<'a> list)(random: int -> int) =
        let lens = seq { for l in reels -> Array.length l }
        randomIdx lens height random |> shoot reels
        
type LineResult<'a> = option<'a * int * bool>
module Line =
    let onePayLine<'a>(snapshot: 'a[][]) =
        Seq.mapi( fun i j -> snapshot.[i].[j])
    
    let payLines<'a> (lines: seq<seq<int>>) (snapshot : 'a[][]) = 
        lines |>> onePayLine snapshot
    
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

    let countLineTwice<'a when 'a: equality> (width:int)(isWild: 'a -> bool)(lineOfSymbol: seq<'a>) =
        let leftResult = countLineOnce isWild lineOfSymbol
        match  leftResult with     
          | Some(_,c,_) when c=width ->
                (leftResult, None)
          | _ ->
                let r2l = lineOfSymbol.Reverse()
                let rightResult = countLineOnce isWild r2l
                (leftResult, rightResult)
        
    let countAllLineTwice<'a when 'a: equality>(width:int)(isWild: 'a->bool)(linesOfSymbol: seq<seq<'a>>) =
       linesOfSymbol |>> countLineTwice width isWild
    
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
            let rl = scanScatter snapshot cs cw
            let er2l = snapshot.Reverse()
            let rr = scanScatter er2l cs cw
            rl,rr
            
module Rtp =
    let rec genStartIdx (lens:list<int>) =    
        match lens with 
        | [] -> Seq.ofList([[]])
        | h::t ->  
            seq { for i in 0..h-1 do
                    for l in (genStartIdx t) do yield i::l }
              
    let genSlice(starts:list<int>)(lens: list<int>)(size:int) =
        List.map2 (fun s l-> Level.safeRings l s size) starts lens