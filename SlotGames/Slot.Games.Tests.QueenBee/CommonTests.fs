module Slot.Games.Tests.QueenBee.CommonTests

open System
open FSharpPlus
open Xunit
open Slot.Games.QueenBee

let pt1 =
    Map [ (5, 100)
          (4, 10)
          (3, 5)
          (2, 1) ]

let pt2 = Map[(9, pt1)]

[<Fact>]
let payTableTest1 () =
    let lookup s = Common.PayTable.simpleLookup s pt1
    let r1 = lookup 5
    Assert.Equal(Some(100), r1)
    let r2 = lookup 1
    Assert.Equal(None, r2)

[<Fact>]
let payTableTest2 () =
    let lookup s c = Common.PayTable.nestedLookup s c pt2

    let r1 = lookup 9 5
    Assert.Equal(Some(100), r1)
    let r1 = lookup 9 2
    Assert.Equal(Some(1), r1)
    let r1 = lookup 9 1
    Assert.Equal(None, r1)
    let r1 = lookup 12 2
    Assert.Equal(None, r1)
    
[<Fact>]
let checkLevel1 () =
    let level = [
        [|1;2;3;4|]
        [|5;6;7;8;9|]
        [|10;11;12|]
    ]
    Assert.Throws<ArgumentException>(
       fun ()-> Common.Level.checkLevel level 3 5 ) |> ignore
    Assert.Throws<ArgumentException>(
       fun ()-> Common.Level.checkLevel level 4 5 ) |> ignore

[<Fact>]
let safePick1 () =
    let reel = [|1;2;3;4;5;6;7;8|]
    
    let r1 = Common.Level.safePick reel [3;5;7]
    Assert.Equal<list<int>>([4;6;8],r1)
   
    let r2 = Common.Level.safePick reel [0;1;2]
    Assert.Equal<list<int>>([1;2;3],r2)   

[<Fact>]
let safeRings1 () =
    let r = Common.Level.safeRings 5 4 3
    Assert.Equal<list<int>>([4;0;1],r)
    
    let r2 = Common.Level.safeRings 5 4 2
    Assert.Equal<list<int>>([4;0],r2)
    
    let r3 = Common.Level.safeRings 5 4 4
    Assert.Equal<list<int>>([4;0;1;2],r3)
    
    let r4 = Common.Level.safeRings 5 3 5
    Assert.Equal<list<int>>([3;4;0;1;2],r4)
    

[<Fact>]
let fakeRandomSeq1 () =
    let f = Common.Test.fakeRandomSeq [3;4;5]
    Assert.Equal(3, f 1)
    Assert.Equal(4, f 2)
    Assert.Equal(5, f 3)
    Assert.Equal(3, f 4)
    Assert.Equal(4, f 5)


[<Fact>]
let randomIdx1 () =
    let random = Common.Test.fakeRandomSeq [3;7;5]
    let r = Common.Level.randomIdx [10;8;7] 3 random
    let e = [
        [3;4;5]
        [7;0;1]
        [5;6;0]
    ]
    Assert.Equal<list<list<int>>>(e, r)
    
[<Fact>]
let randomSpin () =
    let level = [
        [|1;2;3;4|]
        [|5;6;7;8;9|]
        [|10;11;12|]
    ]
    let random = Common.Test.fakeRandomSeq [1;4;0]
    let r = Common.Level.randomSpin 2 level random
    let e = [|
        [|2;3|]
        [|9;5|]
        [|10;11|]
    |]
    Assert.Equal<int[][]>(e, r)
   
let ss = [|[|2;4;7|];[|3;5;2|];[|9;4;0|];[|4;9;2|];[|3;4;1|]|]
let pl1 = [4;5;4;9;4]
let pl2 = [2;3;9;4;3]
let pl3 = [7;2;0;2;1]
let pl4 = [2;5;0;9;3]
let pl5 = [7;5;9;9;1]
let pl6 = [2;3;4;4;3]
let pl7 = [7;2;4;2;1]
let pl8 = [4;2;0;2;4]
let pl9 = [4;3;9;4;4]

[<Fact>]
let payLine () =
    let pl = Common.Line.onePayLine ss
    Assert.Equal<list<int>>(pl1, pl Game.Line.l1)
    Assert.Equal<list<int>>(pl2, pl Game.Line.l2)
    Assert.Equal<list<int>>(pl3, pl Game.Line.l3)
    Assert.Equal<list<int>>(pl4, pl Game.Line.l4)
    Assert.Equal<list<int>>(pl5, pl Game.Line.l5)
    Assert.Equal<list<int>>(pl6, pl Game.Line.l6)
    Assert.Equal<list<int>>(pl7, pl Game.Line.l7)
    Assert.Equal<list<int>>(pl8, pl Game.Line.l8)
    Assert.Equal<list<int>>(pl9, pl Game.Line.l9)
    
   
[<Fact>]
let payLines () =
        let r = Common.Line.payLines Game.Line.allLines ss
        let er = [pl1;pl2;pl3;pl4;pl5;pl6;pl7;pl8;pl9]
        Assert.Equal<list<list<int>>>(er,r)

let equal i =  fun j-> i=j 


[<Fact>]
let countLineOnce () =
    let cf = Common.Line.countLineOnce (equal 3)
    
    let r1 = cf [2;3;4]
    Assert.Equal(Some(2,2,true),r1)
    
    let r2 = cf []
    Assert.Equal(None,r2)
    
    let r3 = cf [3;3;3]
    Assert.Equal(None,r3)
    
    let r4 = cf [4;3;4;5;4]
    Assert.Equal(Some(4,3,true), r4)
    
    let r4 = cf [4;3;3;3;4]
    Assert.Equal(Some(4,5,true), r4)
    
    let r5 = cf [4;5;3;3;4]
    Assert.Equal(Some(4,1,false), r5)
    
    
[<Fact>]
let countLineTwice () =
    let cf w = Common.Line.countLineTwice w (equal 3)
    
    let r1 = cf 3 [2;3;4]
    Assert.Equal((Some(2,2,true),Some(4,2,true)),r1)
    
    let r2 = cf 3 []
    Assert.Equal((None,None),r2)
    
    let r3 = cf 3 [3;3;3]
    Assert.Equal((None,None),r3)
    
    let r4 = cf 5 [4;3;4;5;4]
    Assert.Equal((Some(4,3,true),Some(4,1,false)), r4)
    
    let r4 = cf 5 [4;3;3;3;4]
    Assert.Equal((Some(4,5,true),None), r4)
    
    let r5 = cf 5 [4;5;3;3;4]
    Assert.Equal((Some(4,1,false),Some(4,3,true)), r5)
    
    
[<Fact>]
let countAllLineTwice () =
    let cf w = Common.Line.countAllLineTwice w (equal 4)
    let lines = [
             [2;3;9;4;3]
             [7;2;0;2;1]
    ]
    let er = [
        Some(2,1,false),Some(3,2,true)
        Some(7,1,false),Some(1,1,false)
    ]
    
    let rr = cf 5 lines
    Assert.Equal<Common.LeftRightLineResult<int>>(er, rr)
    
    
[<Fact>]
let countSymbolTest () =
   let count = Common.Line.countSymbol (equal 2)
   Assert.Equal(1,  count [|2;4;7|])
   Assert.Equal(2,  count [|2;2;7|])
   Assert.Equal(3,  count [|2;2;2|])
   Assert.Equal(0,  count [|0;1;3|])
   Assert.Equal(0,  count [||])

[<Fact>]
let scanTest1 () =
   let cs = Common.Line.countSymbol (equal 2)
   let cw = Common.Line.countSymbol (equal 0)
   let r = Common.Line.scanScatter ss cs cw   
   let expected = Some(4, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let scanTest2 () =
   let cs = Common.Line.countSymbol (equal 2)
   let cw = Common.Line.countSymbol (equal 5)
   let r = Common.Line.scanScatter ss cs cw   
   let expected = Some(3, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let scanTest3 () =
   let cs = Common.Line.countSymbol (equal 2)
   let cw = Common.Line.countSymbol (equal 5)
   let r = Common.Line.scanScatter Array.empty cs cw   
   let expected = None
   Assert.Equal(expected, r)

[<Fact>]
let scanTest4 () =
   let cs = Common.Line.countSymbol (equal 10)
   let cw = Common.Line.countSymbol (equal 11)
   let r = Common.Line.scanScatter ss cs cw   
   let expected = None
   Assert.Equal(expected, r)
   
[<Fact>]
let countTest1 () =
   let r = Common.Line.countScatter ss  (equal 4) (equal 5)
   let expected = Some(5, true),Some(5, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let countTest2 () =
   let r = Common.Line.countScatter ss  (equal 5) (equal 4)
   let expected = None, None
   Assert.Equal(expected, r)
 

[<Fact>]
let countTest3 () =
   let r = Common.Line.countScatter ss  (equal 4) (equal 9)
   let expected = Some(1,false), Some(5,true)
   Assert.Equal(expected, r)
  
   
[<Fact>]
let countTest4 () =
   let r = Common.Line.countScatter ss  (equal 3) (equal 9)
   let expected = None, Some(4,true)
   Assert.Equal(expected, r)
   
   
[<Fact>]
let testGenStartIdx1 () =
    let r = Common.Test.genStartIdx [3;3]
    let er = [[0; 0]; [0; 1]; [0; 2]; [1; 0]; [1; 1]; [1; 2]; [2; 0]; [2; 1]; [2; 2]]
    Assert.Equal(er,r)
    Assert.Equal(9, Seq.length r)
    
    let r2 = Common.Test.genStartIdx [5;3;4]
    Assert.Equal(5*3*4, Seq.length r2)
    
[<Fact>]
let genSlice () =
    let r = Common.Test.genSlice [3;4;5] [6;5;7] 3
    let s1 = [3;4;5]
    let s2 = [4;0;1]
    let s3 = [5;6;0]
    let er = [s1;s2;s3]
    Assert.Equal<list<list<int>>>(er,r)
    
    