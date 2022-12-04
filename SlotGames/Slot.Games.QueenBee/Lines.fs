module Slot.Games.QueenBee.Lines

open System.Collections.Generic
open System.Linq
type Line = int[]
let l1 = [|1; 1; 1; 1; 1|]
let l2 = [|0; 0; 0; 0; 0|]
let l3 = [|2; 2; 2; 2; 2|]  
let l4 = [|0; 1; 2; 1; 0|]
let l5 = [|2; 1; 0; 1; 2|]
let l6 = [|0; 0; 1; 0; 0|]
let l7 = [|2; 2; 1; 2; 2|]
let l8 = [|1; 2; 2; 2; 1|]
let l9 = [|1; 0; 0; 0; 1|]

let  allLines = [|l1; l2; l3; l4; l5; l6; l7; l8; l9|]

let onePayLine(snapshot: int[][])(line: Line) =
    line |> Array.mapi( fun i j -> snapshot.[i].[j])
    
let payLines (lines: Line[]) (snapshot : int[][]) = 
    lines |> Array.map (onePayLine snapshot)


let consecutiveCount<'a when 'a : equality>(isWild: 'a->bool)(lineOfSymbol:IEnumerable<'a>) =
  let iter = lineOfSymbol.GetEnumerator()
  if iter.MoveNext() then
      let first = iter.Current
      if (isWild first) then None
      else 
          let mutable consecutive = true
          let mutable count = 1
          while (iter.MoveNext() && consecutive) do
             let e = iter.Current
             if e = first || isWild e then
                 count <- count + 1  
             else
                 consecutive<-false    
          Some(first,count)
  else None
  
  
let countForth2Back<'a when 'a : equality>(isWild: 'a->bool)(lineOfSymbol:'a[]) =
      let l2r = lineOfSymbol.AsEnumerable()
      let leftResult = consecutiveCount isWild l2r
      let r2l = lineOfSymbol.Reverse()
      let rightResult = consecutiveCount isWild r2l
      (leftResult,rightResult) 