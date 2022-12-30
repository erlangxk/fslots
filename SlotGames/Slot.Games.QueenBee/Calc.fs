module Slot.Games.QueenBee.Calc

open FSharpPlus
open Common

let L2R, R2L = "l2r", "r2l"

type IMul =
    abstract myMul: int

type Win =
    { dir: string
      subst: bool
      count: int
      mul: int }

    interface IMul with
        member self.myMul = self.mul

let createWin dir subst c m =
    { dir = dir
      subst = subst
      count = c
      mul = m }

type LineWin<'a> =
    { line: int
      win: Win
      symbol: 'a }

    interface IMul with
        member self.myMul = self.win.mul

type Result<'a> =
    { snapshot: 'a[][]
      scatterWin: list<Win>
      scatterMul: int
      lineWin: list<LineWin<'a>>
      lineMul: int }

type Result<'a> with

    member self.totalMul = self.lineMul + self.scatterMul

let addOneWin<'T when 'T :> IMul> (win: option<'T>) (state: int * list<'T>) =
    let t, ls = state

    match win with
    | None -> state
    | Some(w) -> (t + w.myMul), w :: ls


let lineWinResult<'a>(calLineWin: 'a -> int -> bool -> option<int>)(countAllLines:list<LeftRightLineResult<'a>>) =
     let folder (state: int * list<LineWin<'a>>) (line: int) ((lr, rr): LeftRightLineResult<'a>) =
        let cal dir result =
            monad {
                let! s, c, subst = result
                let! m = calLineWin s c subst

                return
                    { line = line
                      symbol = s
                      win = createWin dir subst c m }
            }

        state |> addOneWin (cal L2R lr) |> addOneWin (cal R2L rr)
     foldi folder (0, []) countAllLines

let computeLineResult<'a>
    (payLines: 'a[][] -> list<list<'a>>)
    (countLineTwice: list<list<'a>> -> list<LeftRightLineResult<'a>>)
    (calcLineWin: 'a -> int -> bool -> option<int>)
    (snapshot: 'a[][])
    =
    snapshot |> payLines |> countLineTwice |> lineWinResult calcLineWin

let computeScatterResult<'a>
    (totalLines: int)
    (countScatter: 'a[][] -> (int * bool) option * (int * bool) option)
    (calScatterWin: int -> bool -> int option)
    (snapshot: 'a[][])
    =
    let lr, rr = countScatter snapshot

    let cal dir result =
        monad {
            let! c, subst = result
            let! m = calScatterWin c subst
            return createWin dir subst c (m * totalLines)
        }

    (0, []) |> addOneWin (cal L2R lr) |> addOneWin (cal R2L rr)

let computeResult<'a> computeScatterResult computeLineResult (snapshot: 'a[][]) =
    let scatterMul, scatterWin = computeScatterResult snapshot
    let lineMul, lineWin = computeLineResult snapshot

    { snapshot = snapshot
      scatterWin = scatterWin
      lineWin = lineWin
      lineMul = lineMul
      scatterMul = scatterMul }
