module Slot.Games.QueenBee.ResultJson

open Slot.Games.QueenBee.Calc
open FSharp.Collections
open Thoth.Json.Net

let encodeWin2Seq(win:Win) =
     seq {
        Encode.string win.dir
        Encode.int win.count
        Encode.int win.mul
       }
let encodeWin (win:Win) =
    encodeWin2Seq win |> Seq.toArray |> Encode.array

let encodeLineWin (lw:LineWin<int>) =
    seq {
       yield Encode.int lw.line
       yield Encode.int lw.symbol
       yield! encodeWin2Seq lw.win
    } |> Seq.toArray |> Encode.array
    
let encodeIntArray(arr:int[]) =
    arr |> Array.map Encode.int |> Encode.array

let encodeIntArray2(arr2:int[][]) =
    arr2 |> Array.map encodeIntArray |> Encode.array

let encodeResult(result:Result<int>) =
    let pairs = seq {
        yield "ss", encodeIntArray2 result.snapshot
        yield "sm", Encode.int result.scatterMul
        yield "lm", Encode.int result.lineMul

        if result.lineMul > 0 then 
           yield "lw", result.lineWin |> List.map encodeLineWin |> Encode.list
     
        if result.scatterMul>0 then 
            yield  "sw", result.scatterWin |> List.map encodeWin |> Encode.list
    }
    pairs |> Seq.toList |> Encode.object |> Encode.toString 0
    