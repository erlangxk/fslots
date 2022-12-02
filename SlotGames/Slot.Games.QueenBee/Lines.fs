module Slot.Games.QueenBee.Lines

type Line = int[]
let l1 = [|1; 1; 1; 1; 1|]
let l2 = [|0; 0; 0; 0; 0|]
let l3 = [|2; 2; 2; 2; 2|]  
let l4 = [|0; 1; 2; 1; 0|]
let l5 = [|2; 1; 0; 1; 2|]
let l6 = [|0; 0; 1; 0; 0|]
let l7 = [|2; 2; 1; 2; 2|]
let l8 = [|0; 1; 1; 1; 0|]
let l9 = [|1; 0; 0; 0; 1|]

let  allLines = [|l1; l2; l3; l4; l5; l6; l7; l8; l9|]

let onePayLine(snapshot: int[][])(line: Line) =
    line |> Array.mapi( fun i j -> snapshot.[i].[j])
    
let payLines (lines: Line[]) (snapshot : int[][]) = 
    lines |> Array.map (onePayLine snapshot)


