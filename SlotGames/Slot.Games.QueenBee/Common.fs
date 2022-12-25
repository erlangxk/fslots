module Slot.Games.QueenBee.Common

open FSharpPlus

type Reel<'a> = 'a[]

module PayTable =
    let simpleLookup = Map.tryFind
    let nestedLookup symbol count = simpleLookup symbol >=> simpleLookup count
        
module Level =
    let checkLevel<'a> (level: Reel<'a> list) (width: int) (height: int) =
        let rsl = List.length level

        if rsl <> width then
            invalidArg (nameof level) $"level should have {width} reels, but got {rsl}"
        else
            for reel in level do
                let rl = Array.length reel

                if rl < height then
                    invalidArg (nameof level) $"reel of level should NOT less than {height}, but got {rl}"

    let safePick<'a> (reel:Reel<'a>)(idx: list<int>) = idx |>> Array.get reel 

    let safeRings (len: int) (start: int) (size: int) =
        [ for i in start .. size + start - 1 -> i % len ]
    
    let randomIdx (lens: list<int>)(height: int)(random: int -> int) =
        [ for len in lens -> safeRings len (random len) height ]

    let shoot(reels: Reel<'a> list)(reelIndex: list<list<int>>) =
        let oneReel reel is =  (safePick reel is) |> List.toArray
        List.map2 oneReel reels reelIndex |> List.toArray
    
    let randomSpin<'a> (height: int)(reels: Reel<'a> list)(random: int -> int) =
        let lens = [ for l in reels -> Array.length l ]
        randomIdx lens height random |> shoot reels
        
type LineResult<'a> = option<'a * int * bool>
type LeftRightLineResult<'a> = LineResult<'a> * LineResult<'a>
type AllLineResult<'a> = list<LeftRightLineResult<'a>>

module Line =
    let onePayLine<'a>(snapshot: 'a[][])=
        List.mapi( fun i j -> snapshot[i][j])
    
    let payLines<'a> (lines: list<list<int>>) (snapshot : 'a[][]) = 
        lines |>> onePayLine snapshot
    
    let countLineOnce<'a when 'a: equality> (isWild: 'a -> bool) (lineOfSymbol: list<'a>):LineResult<'a> =
        let take first rest = rest |> List.takeWhile (fun s-> s= first || isWild s) 
        let fold s list =
            let c,r = list |> List.fold (fun (c,r) e -> (c+1,(r || isWild e))) (0,false)
            s,c+1,r
        let check s = if isWild s then None else Some(s)
        let run s = List.tail lineOfSymbol |> take s |> fold s
        List.tryHead lineOfSymbol >>= check |>> run
    
    let countLineTwice<'a when 'a: equality> (width:int)(isWild: 'a -> bool)(lineOfSymbol: list<'a>):LeftRightLineResult<'a> =
        let leftResult = countLineOnce isWild lineOfSymbol
        match  leftResult with     
          | Some(_,c,_) when c=width ->
                (leftResult, None)
          | _ ->
                let rightResult = countLineOnce isWild (List.rev lineOfSymbol)
                (leftResult, rightResult)
        
    let countAllLineTwice<'a when 'a: equality>(width:int)(isWild: 'a->bool)(linesOfSymbol: list<list<'a>>):AllLineResult<'a> =
       linesOfSymbol |>> countLineTwice width isWild
    
    let countSymbol<'a> (test:'a->bool) =
        Array.sumBy (fun x -> if test x then 1 else 0)
    
    let scanScatter (snapshot: 'a[][]) (countScatter: 'a[] -> int) (countWild: 'a[] -> int) =
        let countRest = Array.map (fun reel -> (countScatter reel, countWild reel)) 
        let fold fc counts =
            let c,r = counts |> Array.fold (fun (c,r) (ts,tw)-> (c+ts+tw, r||(tw>0))) (0,false)
            fc+c,r
        let countFirst (first:'a[]) =
            let c = countScatter first
            if c=0 then None else Some(c)
        let run fc = Array.tail snapshot  |> countRest |> takeWhile (fun (s,w)-> s+w>0) |> fold fc
        Array.tryHead snapshot >>= countFirst |>> run    
    
    let countScatter<'a> (snapshot: 'a[][]) (isScatter:'a->bool) (isWild: 'a->bool) =
            let cs = countSymbol isScatter
            let cw = countSymbol isWild
            let rl = scanScatter snapshot cs cw
            let rr = scanScatter (Array.rev snapshot) cs cw
            rl,rr
            
module Test =
    let fakeRandomSeq (numbers:seq<int>) =
         let state = numbers.GetEnumerator()
         let rec f (i:int) =
             if state.MoveNext() then
                 state.Current
             else
                 state.Reset()
                 f i
         f
    let rec genStartIdx (lens:list<int>) =    
        match lens with 
        | [] -> Seq.ofList([[]])
        | h::t ->  
            seq { for i in 0..h-1 do
                    for l in (genStartIdx t) do yield i::l }
              
    let genSlice(starts:list<int>)(lens: list<int>)(size:int) =
        List.map2 (fun s l-> Level.safeRings l s size) starts lens