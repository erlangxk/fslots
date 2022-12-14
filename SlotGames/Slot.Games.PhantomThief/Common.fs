namespace Slot.Games.PhantomThief

open Slot.Game.Prelude
open FSharpPlus
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Slot.Games.PhantomThiefTests")>]
do ()

module Common =

    let internal BONUS_MIN_COUNTS = 3

    type Idx = int * int

    let countBonus<'a when 'a: equality> (isBonus: 'a -> bool) (sequence: list<int * int * 'a>) =
        [ for i, j, e in sequence do
              if (isBonus e) then
                  yield (i, j) ]

    let allBonusIdx (idx: list<int * int>) =
        if (idx.Length < BONUS_MIN_COUNTS) then [] else idx

    let internal countGems<'a when 'a: comparison> (gems: list<'a>) (snapshot: 'a[][]) =
        let result =
            seq {
                for i in 0 .. snapshot.Length - 1 do
                    for g in gems do
                        let oj = Array.tryFindIndex ((=) g) snapshot[i]

                        if oj.IsSome then
                            yield (i, oj.Value, g)
            }
            
        result
        |> Seq.groupBy (fun (_, _, s) -> s)
        |> Map.ofSeq
        |> Map.mapValues (Seq.map (fun (i, j, _) -> (i, j)) >> List.ofSeq)

    type GemWin<'a> = 'a * list<Idx> * int
    type GemWinResult<'a> = list<GemWin<'a>>
    type TotalGemWinResult<'a> = int * GemWinResult<'a>

    let allGemsIdx (gemsResult: GemWinResult<'a>) =
        gemsResult |> Seq.collect (fun (_, ls, _) -> ls)

    let internal calcGemsMul<'a when 'a: comparison>
        (payTable: Map<'a, Map<int, int>>)
        (result: Map<'a, list<Idx>>)
        : TotalGemWinResult<'a> =
        let folder (tm, ls) gem pos =
            let c = List.length pos
            let mul = Core.getNestedMultiplier gem c payTable

            match mul with
            | Some(m) -> (tm + m, (gem, pos, m) :: ls)
            | None -> (tm, ls)

        Map.fold folder (0, []) result

    let countGemsResult<'a when 'a: comparison> (gems: list<'a>) (payTable: Map<'a, Map<int, int>>) (snapshot: 'a[][]) =
        snapshot |> countGems gems |> calcGemsMul payTable

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
