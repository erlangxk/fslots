namespace Slot.Games.PhantomThief

open Slot.Game.Prelude
open FSharpPlus
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Slot.Games.PhantomThiefTests")>]
do ()

module Common =

    let randomFreeGame (rng: unit -> float) =
        let r = rng ()

        if r <= 0.3350 then 2
        elif r <= 0.6500 then 3
        else 4

    type Idx = int * int
    
    let countBonus<'a when 'a: equality> (isBonus: 'a -> bool) (snapshot: 'a[][]) =
        let all =
            seq {
                for i in 0 .. snapshot.Length - 1 do
                    for j in 0 .. snapshot[i].Length - 1 do
                        (i, j, snapshot[i][j])
            }
        all |> Seq.filter (fun (_,_,e) -> isBonus e) |> Seq.map (fun (i,j,_) -> i,j) |> Seq.toList

    let allBonusIdx(idx:list<int*int>) = if(idx.Length >=3) then idx else []
    
    let internal countGems<'a when 'a: comparison> (gems: list<'a>) (snapshot: 'a[][]) =
        let all =
            seq {
                for i in 0 .. snapshot.Length - 1 do
                    for j in 0 .. snapshot[i].Length - 1 do
                        (i, j, snapshot[i][j])
            }

        let folder (state: Map<'a, list<Idx>>) (i, j, e) =
            match Map.tryFind e state with
            | Some(l) -> state |> Map.add e ((i, j) :: l)
            | None -> state

        let init = gems |> List.map (fun g -> g, List.empty<Idx>) |> Map.ofList

        Seq.fold folder init all

    type GemWin<'a> = 'a * list<Idx> * int
    type GemWinResult<'a> = list<GemWin<'a>>
    type TotalGemWinResult<'a> = int * GemWinResult<'a>

    let allGemsIdx (gemsResult: GemWinResult<'a>) =
        seq {
            for (_, ls, _) in gemsResult do
                yield! ls
        }
        
   

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
        countGems gems snapshot |> calcGemsMul payTable

    type LineWin<'a> = int * 'a * int * int
    type LineWinResult<'a> = list<LineWin<'a>>
    type TotalLineWinResult<'a> = int * LineWinResult<'a>
    
    let lineWinResult<'a>(calcMul: 'a -> int -> option<int>)(lineResult:list<Core.LineResult<'a>>): TotalLineWinResult<'a> =
        let folder(state: int * LineWinResult<'a>)(line: int)(r: Core.LineResult<'a>)=
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
        } |> Seq.distinct

    type Action =
        | Spin
        | Cascade

    type GameState =
        { mainGame: bool
          freeSpin: int
          name: string
          idxMatrix: list<list<int>>
          snapshot: int[][]
          lineMul: int
          lineResult: LineWinResult<int>
          gemsMul: int
          gemsResult: GemWinResult<int>
          bonus: list<int*int> }
