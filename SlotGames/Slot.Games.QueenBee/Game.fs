module Slot.Games.QueenBee.Game

module Level =
    let width = 5
    let height = 3
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
    
    Common.Level.checkLevel l1 width height
    Common.Level.checkLevel l2 width height
    Common.Level.checkLevel l3 width height
    
    let queenBeeSpin<'a> (level: 'a[] list)  (random :int -> int) = Common.Level.randomSpin level height random
    let queenBeeSpinLevel1 (random :int -> int) = queenBeeSpin l1 random
    let queenBeeSpinLevel2 (random :int -> int) = queenBeeSpin l2 random
    let queenBeeSpinLevel3 (random :int -> int) = queenBeeSpin l3 random
module PayTable =
    let Ten = 0
    let Jack = 1
    let Queen = 2
    let King = 3
    let Ace = 4
    let Daisy = 5
    let Flower =6
    let BeeHives = 7
    let SkinnyBee = 8
    let FatBee = 9
    let Scatter = 10
    let Wild = 11
    
    let fatBee    = Map [(5,5000);(4,500);(3,100);(2,10);(1,2)]
    let skinnyBee = Map [(5,2500);(4,250);(3,50);(2,5)]
    let beeHives =  Map [(5,1000);(4,100);(3,20);(2,3)]
    let flower   =  Map [(5,1000);(4,100);(3,20);(2,3)]
    let daisy = Map [(5,500);(4,30);(3,10)]
    let ace   = Map [(5,300);(4,25);(3,5)]
    let king  = Map [(5,200);(4,20);(3,5)]
    let queen = Map [(5,200);(4,20);(3,5)]
    let jack  = Map [(5,100);(4,15);(3,5)]
    let ten   = Map [(5,100);(4,15);(3,5)]

    let scatter = Map [(5,100);(4,10);(3,5);(2,1)]

    let plainPayTable:Map<int, Map<int,int>> = Map[
        FatBee,fatBee;
        SkinnyBee, skinnyBee;
        BeeHives, beeHives;
        Flower, flower;
        Daisy, daisy;
        Ace, ace;
        King, king;
        Queen, queen;
        Jack, jack;
        Ten, ten
    ]
    let queenBeeScatterWin  = Common.PayTable.simpleLookup scatter
    let queenBeePlainWin  = Common.PayTable.nestedLookup plainPayTable 

module Line =
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
    
    let queenBeePayLines<'a> (snapshot:'a[][])  = Common.Line.payLines allLines snapshot
    
