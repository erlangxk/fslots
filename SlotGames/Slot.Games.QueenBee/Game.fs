namespace Slot.Games.QueenBee

open FSharpPlus
open Common
open Slot.Game.Prelude

module Level =
    let width,height = 5,3
    
    //RTP = 0.9585549701566001
    let l1 = [
        [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
        [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3|];
        [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
        [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
        [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|]
    ]
    let lens1 = Core.lens l1
    
    //RTP = 0.9339259632022288
    let l2 = [
          [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
          [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3;0;0;0|];
          [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
          [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
          [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|] 
    ]
    
    let lens2 = Core.lens l2

    //RTP = 0.9057785266780308
    let l3 = [
          [|1;4;10;2;4;7;0;1;8;3;5;7;2;3;4;8;2;3;7;8;3;1;6;0;2;9;3;2;7;6;2;5;3;4;7;3;4;5;2;6;3;3;3|];
          [|2;4;10;0;3;5;2;0;7;2;4;11;1;4;9;3;4;5;1;4;0;1;3;5;2;6;4;2;8;3;2;7;4;2;5;1;2;4;2;0;0;3;0;0;0;0;0;0;0|];
          [|4;1;10;0;2;9;4;0;6;1;3;11;0;1;5;0;2;6;0;8;3;0;7;3;9;1;0;6;8;0;3;9;0;5;3;4;5;0;2;6|];
          [|3;5;10;4;8;2;4;9;2;1;11;2;8;6;0;3;6;5;2;6;4;1;7;2;5;3;1;7;2;6;4;1;8;3;6;0;1|];
          [|2;0;10;3;6;7;0;3;4;1;5;0;4;7;2;5;4;9;7;2;4;5;0;1;7;2;4;7;2;0;8;1;5;6|]
    ]
    let lens3 = Core.lens l3
    
    Core.checkReels l1 width height
    Core.checkReels l2 width height
    Core.checkReels l3 width height
    
    let private queenBeeSpin = Core.randomSnapshot height
    let spinLevel1 = queenBeeSpin l1 lens1
    let spinLevel2 = queenBeeSpin l2 lens2
    let spinLevel3 = queenBeeSpin l3 lens3
    
module PayTable =
    let Ten,Jack,Queen,King,Ace = 0,1,2,3,4
    let Daisy,Flower,BeeHives,SkinnyBee,FatBee= 5,6,7,8,9
    let Scatter,Wild = 10,11
    
    let linePayTable = Map[
        FatBee,Map [(5,5000);(4,500);(3,100);(2,10);(1,2)];
        SkinnyBee, Map [(5,2500);(4,250);(3,50);(2,5)];
        BeeHives, Map [(5,1000);(4,100);(3,20);(2,3)];
        Flower, Map [(5,1000);(4,100);(3,20);(2,3)];
        Daisy, Map [(5,500);(4,30);(3,10)];
        Ace, Map [(5,300);(4,25);(3,5)];
        King, Map [(5,200);(4,20);(3,5)];
        Queen, Map [(5,200);(4,20);(3,5)];
        Jack, Map [(5,100);(4,15);(3,5)];
        Ten, Map [(5,100);(4,15);(3,5)]
    ]
   
    let scatterPayTable = Map [(5,100);(4,10);(3,5);(2,1)]
    let multiply2 subst value = if subst then value*2 else value
    let calScatterWin count subst  =
        scatterPayTable |> Core.getMultiplier count |>> multiply2 subst 
    let calPlainWin symbol count subst  =
        linePayTable |> Core.getNestedMultiplier symbol count |>> multiply2 subst
    let queenBeeIsWild e = e = Wild
    let queenBeeIsScatter e = e = Scatter

module Line =
    let l1 = [1; 1; 1; 1; 1]
    let l2 = [0; 0; 0; 0; 0]
    let l3 = [2; 2; 2; 2; 2]                     
    let l4 = [0; 1; 2; 1; 0]
    let l5 = [2; 1; 0; 1; 2]
    let l6 = [0; 0; 1; 0; 0]
    let l7 = [2; 2; 1; 2; 2]
    let l8 = [1; 2; 2; 2; 1]
    let l9 = [1; 0; 0; 0; 1]
    
    let allLines = [l1; l2; l3; l4; l5; l6; l7; l8; l9]
    let totalLines = allLines.Length
    let queenBeePayLines  = Core.allPayLines allLines
    let queenBeeCountAllLineTwice = countAllLineTwice Level.width PayTable.queenBeeIsWild
    let queenBeeCountScatter snapshot =
        countScatter snapshot PayTable.queenBeeIsScatter PayTable.queenBeeIsWild

module Pack =

    let queenBeeComputeLineResult =
        Calc.computeLineResult Line.queenBeePayLines Line.queenBeeCountAllLineTwice PayTable.calPlainWin

    let queenBeeComputeScatterResult =
        Calc.computeScatterResult Line.totalLines Line.queenBeeCountScatter PayTable.calScatterWin

    let computeResult =
        Calc.computeResult queenBeeComputeScatterResult queenBeeComputeLineResult

    let randomSpinLevel1 (random: int -> int) =
        Level.spinLevel1 random |> computeResult


    let meta =
        {| name = "queenBee"
           width = Level.width
           height = Level.height
           lines = Line.allLines |}