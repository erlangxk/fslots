namespace Slot.Game.Prelude

open FSharpPlus

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Slot.Game.PreludeTests")>]
do ()


module Core =
    type Reel<'a> = 'a[]

    let lens (reels: Reel<'a> list) = [ for l in reels -> Array.length l ]

    let internal idxRing (len: int) (start: int) (size: int) =
        [ for i in start .. size + start - 1 -> i % len ]

    let randomReelIdx (lens: list<int>) (height: int) (rng: int -> int) =
        [ for len in lens -> idxRing len (rng len) height ]

    let internal cherryPick<'a> (reel: Reel<'a>) (idx: list<int>) = idx |>> Array.get reel

    type Snapshot<'a> = 'a[][]
    
    let lineup<'a> (ss: Snapshot<'a>) =
        seq {
            for i in 0 .. ss.Length - 1 do
                for j in 0 .. ss[i].Length - 1 do
                    (i, j, ss[i][j])
        } |> Seq.toList

    let snapshot (reels: Reel<'a> list) (reelIdx: list<list<int>>) : Snapshot<'a> =
        let oneReel reel idx = (cherryPick reel idx) |> List.toArray
        List.map2 oneReel reels reelIdx |> List.toArray

    let randomSnapshot<'a> (height: int) (reels: Reel<'a> list) (lens: list<int>) (random: int -> int) =
        randomReelIdx lens height random |> snapshot reels

    let internal onePayLine<'a> (snapshot: 'a[][]) = List.mapi (fun i j -> snapshot[i][j])

    let allPayLines<'a> (lines: list<list<int>>) (snapshot: 'a[][]) = lines |>> onePayLine snapshot

    type LineResult<'a> = option<'a * int * bool>

    let countConsecutiveSymbolsNoWild<'a when 'a: equality> (lineOfSymbol: list<'a>) : LineResult<'a> =
        let run s =
            let c = List.tail lineOfSymbol |> List.takeWhile (fun e -> e = s) |> List.length
            s, c+1, false
            
        List.tryHead lineOfSymbol |>> run

    let countConsecutiveSymbols<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: list<'a>) : LineResult<'a> =
        let take first rest =
            rest |> List.takeWhile (fun s -> s = first || isWild s)

        let fold s list =
            let c, r = list |> List.fold (fun (c, r) e -> (c + 1, (r || isWild e))) (0, false)
            s, c + 1, r

        let check s = if isWild s then None else Some(s)

        let run s =
            List.tail lineOfSymbol |> take s |> fold s

        List.tryHead lineOfSymbol >>= check |>> run

    let checkReels<'a> (level: Reel<'a> list) (width: int) (height: int) =
        let rsl = List.length level

        if rsl <> width then
            invalidArg (nameof level) $"level should have {width} reels, but got {rsl}"
        else
            for reel in level do
                let rl = Array.length reel

                if rl < height then
                    invalidArg (nameof level) $"reel of level should NOT less than {height}, but got {rl}"


    let getMultiplier (count: int) (payTable: Map<int, int>) = Map.tryFind count payTable

    let getNestedMultiplier<'a when 'a: comparison> (symbol: 'a) (count: int) (payTable: Map<'a, Map<int, int>>) =
        payTable |> (Map.tryFind symbol >=> Map.tryFind count)

    let countSymbol<'a> (test: 'a -> bool) =
        Array.sumBy (fun x -> if test x then 1 else 0)
