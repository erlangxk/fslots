namespace Slot.Game.Common

open FSharpPlus

module Core =
    type Reel<'a> = 'a[]
    let idxRing (len: int) (start: int) (size: int) =
        [ for i in start .. size + start - 1 -> i % len ]
    
    let randomReelIdx (lens: list<int>)(height: int)(rng: int -> int) =
        [ for len in lens -> idxRing len (rng len) height ]

    let cherryPick<'a> (reel:Reel<'a>)(idx: list<int>) = idx |>> Array.get reel
    
    type Snapshot<'a> = 'a[][]
    
    let snapshot(reels: Reel<'a> list)(reelIdx: list<list<int>>):Snapshot<'a> =
        let oneReel reel idx =  (cherryPick reel idx) |> List.toArray
        List.map2 oneReel reels reelIdx |> List.toArray
    
    let randomSnapshot<'a> (height: int)(reels: Reel<'a> list)(random: int -> int) =
        let lens = [ for l in reels -> Array.length l ]
        randomReelIdx lens height random |> snapshot reels

    let onePayLine<'a>(snapshot: 'a[][])=
        List.mapi( fun i j -> snapshot[i][j])
    
    let allPayLines<'a> (lines: list<list<int>>) (snapshot : 'a[][]) = 
        lines |>> onePayLine snapshot

    
    type LineResult<'a> = option<'a * int * bool>

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


    let getMultiplier (count:int) (payTable:Map<int,int>) = Map.tryFind count payTable
    
    let getNestedMultiplier<'a when 'a:comparison> (symbol: 'a) (count:int) (payTable:Map<'a,Map<int,int>>) =
        payTable |> (Map.tryFind symbol >=> Map.tryFind count)