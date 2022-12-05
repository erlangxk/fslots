module Slot.Games.QueenBee.Levels

open FSharpPlus

type Reel = int[]
let l1 = [
    [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
    [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3|];
    [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
    [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
    [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|]
]

let l2 = [
      [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
      [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3;0;0;0|];
      [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
      [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
      [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|] 
]

let l3 = [
      [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
      [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3;0;0;0;0;0;0;0|];
      [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
      [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
      [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|]
]

let width = 5
let height = 3

let safeRings(len:int)(start:int)(size:int) =
    seq { for i in start .. size+start-1 -> i % len }

let rings (len:int) (start:int) (size:int) =
      if start >=0 && start< len  && size>0 && size <= len then
           Some(safeRings len start size)
      else None

let safeSpinOneLine (reel:Reel) (idx:seq<int>) =
      map (fun i -> reel[i]) idx

let spinOneLine (size:int) (reel: Reel) (start:int)= 
       map (safeSpinOneLine reel) (rings reel.Length start size)

let rec mapOption2 f list1 list2 =
    match list1,list2 with
    | [],[] ->
        Some []
    | [],_::_ -> 
        None
    |_::_,[]->
        None
    | h1::t1, h2::t2 ->
        (lift2 List.cons) (f h1 h2) (mapOption2 f t1 t2) 

let spin (reels: Reel list) (starts: int list) (size:int ) =
      if reels.Length <> starts.Length then None
      else mapOption2 (spinOneLine size) reels starts
                  
let queenBeeSpin (lists: Reel list)  (starts :int list) =
      if lists.Length <> width|| starts.Length <> width  then None
      else spin lists starts height