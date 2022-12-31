namespace Slot.Games.PhantomThief

open Slot.Game.Prelude
open FSharpPlus
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Slot.Games.PhantomThiefTests")>]
do ()

module Common =

    type Idx = int * int

    let countBonus<'a when 'a: equality> (isBonus: 'a -> bool) (sequence: list<int*int*'a>) =
        seq {
            for i, j, e in sequence do
                if (isBonus e) then
                    yield (i, j)
        }
        |> Seq.toList

    let allBonusIdx (idx: list<int * int>) = if (idx.Length < 3) then [] else idx
     
    let internal countGems<'a when 'a: comparison> (gems: list<'a>) (sequence: list<int*int*'a>) =
        let folder (state: Map<'a, list<Idx>>) (i, j, e) =
            match Map.tryFind e state with
            | Some(l) -> state |> Map.add e ((i, j) :: l)
            | None -> state

        let init = gems |> List.map (fun g -> g, List.empty<Idx>) |> Map.ofList

        sequence |> Seq.fold folder init

    type GemWin<'a> = 'a * list<Idx> * int
    type GemWinResult<'a> = list<GemWin<'a>>
    type TotalGemWinResult<'a> = int * GemWinResult<'a>

    let allGemsIdx (gemsResult: GemWinResult<'a>) =
        gemsResult |> Seq.collect (fun (_,ls,_) -> ls)
 
    let internal calcGemsMul<'a when 'a: comparison>
        (payTable: Map<'a, Map<int, int>>)
        (result: Map<'a, list<Idx>>)
        : TotalGemWinResult<'a> =
        let folder (tm, ls) gem pos =
            let c = List.length pos
            
            let z = if c>5 then 5 else c
            let mul = Core.getNestedMultiplier gem z payTable

            match mul with
            | Some(m) -> (tm + m, (gem, pos, m) :: ls)
            | None -> (tm, ls)

        Map.fold folder (0, []) result

    let countGemsResult<'a when 'a: comparison> (gems: list<'a>) (payTable: Map<'a, Map<int, int>>) (sequence: list<int*int*'a>) =
        countGems gems sequence |> calcGemsMul payTable

    type LineWin<'a> = int * 'a * int * int
    type LineWinResult<'a> = list<LineWin<'a>>
    type TotalLineWinResult<'a> = int * LineWinResult<'a>

    let lineWinResult<'a>
        (calcMul: 'a -> int -> option<int>)
        (lineResult: list<Core.LineResult<'a>>)
        : TotalLineWinResult<'a> =
        let folder (state: int * LineWinResult<'a>) (line: int) (r: Core.LineResult<'a>) =
            let win result =
                monad {
                    let! s, c, _ = result
                    let! m = calcMul s c
                    return (line, s, c, m)
                }

            let t, ls = state

            match win r with
            | None -> state
            | Some(_, _, _, m as w) -> (t + m), w :: ls

        foldi folder (0, []) lineResult

    let computeLineResult<'a>
        (payLines: 'a[][] -> list<list<'a>>)
        (countLine: list<list<'a>> -> list<Core.LineResult<'a>>)
        (calcMul: 'a -> int -> option<int>)
        (snapshot: 'a[][])
        : TotalLineWinResult<'a> =
        snapshot |> payLines |> countLine |> lineWinResult calcMul


    let allLineIdx<'a> (lines: Map<int, list<int>>) (lineResult: LineWinResult<'a>) =
        seq {
            for (line, _, count, _) in lineResult do
                let ls = Map.find line lines
                yield! ls |> Seq.take count |> Seq.mapi (fun i j -> i, j)
        }
        |> Seq.distinct
