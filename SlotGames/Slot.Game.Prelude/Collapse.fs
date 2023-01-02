module Slot.Game.Prelude.Collapse

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Slot.Game.PreludeTests")>]
do ()

let private TAG_REMOVE = -1

let internal removeIdx(matrix:list<list<int>>)(idxToRemove:seq<int*int>)=
    let arrMatrix = matrix |> List.map List.toArray |> List.toArray 
    idxToRemove |> Seq.iter (fun (i,j)-> arrMatrix[i][j] <- TAG_REMOVE)
    arrMatrix
    |> Array.map (fun arr-> Array.filter (fun i-> i <> TAG_REMOVE) arr |> Array.toList)
    |> Array.toList

let internal reloadReel (ol:list<int>) (nl:list<int>) (l:int) =
        let next = ol.Head
        let size = ol.Length - nl.Length
        [for i in size.. -1 ..1 do yield (next-i+l) % l] @ nl
                   
let internal reloadIdxMatrix(oldMatrix:list<list<int>>)(newMatrix:list<list<int>>)(lens:list<int>)=
   List.map3 reloadReel oldMatrix newMatrix lens
   
let collapse(matrix:list<list<int>>)(idx:seq<int*int>)(lens:list<int>) =
    let nm = removeIdx matrix idx
    reloadIdxMatrix matrix nm lens