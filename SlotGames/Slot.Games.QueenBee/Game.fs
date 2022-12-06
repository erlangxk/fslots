module Slot.Games.QueenBee.Game

open FSharpPlus.Data
open Microsoft.FSharp.Core
open FSharpPlus
open Common
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
    
    let queenBeeSpin = Common.Level.randomSpin height
    let queenBeeSpinLevel1  = queenBeeSpin l1
    let queenBeeSpinLevel2  = queenBeeSpin l2
    let queenBeeSpinLevel3  = queenBeeSpin l3
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

    let plainPayTable = Map[
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
    let queenBeeScatterWin  = PayTable.simpleLookup scatter
    let queenBeePlainWin  = PayTable.nestedLookup plainPayTable
    
    let queenBeeIsWild e = e = Wild
    let queenBeeIsScatter e = e = Scatter

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
    
    let queenBeePayLines  = Line.payLines allLines
    
    let queenBeeCountAllLineTwice = Line.countAllLineTwice PayTable.queenBeeIsWild
    
    let queenBeeCountScatter snapshot = Line.countScatter snapshot PayTable.queenBeeIsScatter PayTable.queenBeeIsWild
module Core =
    
    type LineResultTwice<'a> = seq<LineResult<'a> option * LineResult<'a> option>
    type Result<'a> = {
        snapshot: 'a[][]
        lines: 'a[][]
        linesResult: LineResultTwice<'a>
        plainWin: seq<int option * int option>
        scatterResult:(int*bool) option *  (int*bool) option
        scatterWin: int option * int option
        multiplier:int
    }
    let spinLevel1 (random :int -> int) =
        let ss = Level.queenBeeSpinLevel1(random)
 
        let payLines = Line.queenBeePayLines ss
        
        let len = payLines.Length
        
        let countAllLines = Line.queenBeeCountAllLineTwice payLines |> Seq.ofArray
        
        let plainWin = seq {
            for line in countAllLines do
                let l,r = line
                let lm = monad {
                    let! s,c,w = l
                    let! m = PayTable.queenBeePlainWin s c
                    return if w then m*2 else m
                }
                let rm = monad {
                    let! s,c,w = r
                    let! m = PayTable.queenBeePlainWin s c
                    return if w then m*2 else m
                }
                yield lm,rm
        }
        let countScatter = Line.queenBeeCountScatter ss
        let scatterWin =
            let (sl, sr) = countScatter
            
            let slm = monad {
                let! c,w = sl
                let! m = PayTable.queenBeeScatterWin c
                return if w then m*2 else m
            }
                
            let srm =monad {
                let! c,w = sl
                let! m = PayTable.queenBeeScatterWin c
                return if w then m*2 else m
            }
            slm,srm
        
        let folder s e= s + e*len 
        
        let slm,srm = scatterWin
        let mslm =  Option.fold folder 0 slm
        let msrm =  Option.fold folder 0 srm
        
        let folder2 s e =
            let l,r = e
            let ml =  Option.fold folder 0 l
            let mr =  Option.fold folder 0 r
            ml+mr+s
            
        let pwm = Seq.fold folder2 0 plainWin
        {
            snapshot = ss
            lines = payLines
            linesResult = countAllLines
            plainWin = plainWin
            scatterResult = countScatter
            scatterWin = scatterWin
            multiplier = mslm + msrm + pwm
        }