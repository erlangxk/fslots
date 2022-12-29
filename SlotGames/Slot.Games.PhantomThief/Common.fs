namespace Slot.Games.PhantomThief

open Slot.Game.Prelude
open FSharpPlus

module Common =

    let randomFreeGame (r: decimal) =
        if r <= 0.3350m then 2
        elif r <= 0.6500m then 3
        else 4

    let countBonus<'a when 'a: equality> (isBonus: 'a -> bool) (snapshot: 'a[][]) =
        snapshot |> Seq.fold (fun count arr -> count + Core.countSymbol isBonus arr) 0

    type Pos = int * int

    let countGems<'a when 'a: comparison> (gems: list<'a>) (snapshot: 'a[][]) =
        let all =
            seq {
                for i in 0 .. snapshot.Length - 1 do
                    for j in 0 .. snapshot[i].Length - 1 do
                        (i, j, snapshot[i][j])
            }

        let folder (state: Map<'a, list<Pos>>) (i, j, e) =
            match Map.tryFind e state with
            | Some(l) -> state |> Map.add e ((i, j) :: l)
            | None -> state

        let init = gems |> List.map (fun g -> g, List.empty<Pos>) |> Map.ofList

        Seq.fold folder init all

    let calcGemsMul(payTable: Map<'a, Map<int, int>>) (result: Map<'a, list<Pos>>)  =
        seq {
            for kv in result do
                let c = List.length kv.Value
                let m = Core.getNestedMultiplier kv.Key c payTable
                if m.IsSome then
                    yield kv.Key, kv.Value, m.Value
        }

    let countGemsWin<'a when 'a: comparison> (gems: list<'a>)(payTable: Map<'a, Map<int, int>>)(snapshot: 'a[][]) =
        countGems gems snapshot |> calcGemsMul payTable
    
    type LineWin<'a> = int * 'a * int * int

    let computeLineResult<'a>
        (payLines: 'a[][] -> list<list<'a>>)
        (countLine: list<list<'a>> -> list<Core.LineResult<'a>>)
        (calcMul: 'a -> int -> option<int>)
        (snapshot: 'a[][])
        =
        let folder (state: int * list<LineWin<'a>>) (line: int) (r: Core.LineResult<'a>) =
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

        snapshot |> payLines |> countLine |> (foldi folder (0, []))


    let winLineCount<'a> (lines: Map<int, list<int>>) (lineWins: list<LineWin<'a>>) : list<Collapse.LineCount> =
        lineWins
        |> List.map (fun (line, _, count, _) ->
            let l = Map.find line lines
            l, count)
