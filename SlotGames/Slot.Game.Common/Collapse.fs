module Slot.Game.Common.Collapse



type LineCount =   list<int> * int

let labelIdxMatrix(matrix:list<list<int>>)(lines:List<LineCount>)=
    let arrMatrix = matrix |> List.map List.toArray |> List.toArray 
    lines |> Seq.iter (fun (ls,c) -> 
       let action =  Seq.take c >> Seq.iteri (fun i j -> arrMatrix[i][j]<- -1) 
       ls |> action
    )
    arrMatrix
    |> Seq.map (fun arr-> Seq.filter (fun i-> i <> -1) arr |> Seq.toList)
    |> Seq.toList

let reload (ol:list<int>) (nl:list<int>) (l:int) =
        let next = ol.Head
        let size = ol.Length - nl.Length
        [for i in size.. -1 ..1 do yield (next-i+l) % l] @ nl
                   
let reloadMatrix(oldMatrix:list<list<int>>)(newMatrix:list<list<int>>)(lens:list<int>)=
   List.map3 reload oldMatrix newMatrix lens
   
let collapse(matrix:list<list<int>>)(lines:List<LineCount>)(lens:list<int>) =
    let nm = labelIdxMatrix matrix lines
    reloadMatrix matrix nm lens