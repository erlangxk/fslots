module Slot.Games.Tests.QueenBee.CommonTests

open System
open System.Collections.Generic
open System.Linq
open NUnit.Framework
open Slot.Games.QueenBee


let pt1 =
    Map [ (5, 100)
          (4, 10)
          (3, 5)
          (2, 1) ]

let pt2 = Map[(9, pt1)]

[<Test>]
let payTableTest1 () =
    let lookup =
        Common.PayTable.simpleLookup pt1

    let r1 = lookup 5
    Assert.AreEqual(Some(100), r1)
    let r2 = lookup 1
    Assert.AreEqual(None, r2)

[<Test>]
let payTableTest2 () =
    let lookup =
        Common.PayTable.nestedLookup pt2

    let r1 = lookup 9 5
    Assert.AreEqual(Some(100), r1)
    let r1 = lookup 9 2
    Assert.AreEqual(Some(1), r1)
    let r1 = lookup 9 1
    Assert.AreEqual(None, r1)
    let r1 = lookup 12 2
    Assert.AreEqual(None, r1)
    
[<Test>]
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

[<Test>]
let safePick1 () =
    let reel = [|1;2;3;4;5;6;7;8|]
    
    let r1 = Common.Level.safePick reel [3;5;7]
    Assert.AreEqual([4;6;8],r1)
   
    let r2 = Common.Level.safePick reel [0;1;2]
    Assert.AreEqual([1;2;3],r2)   

[<Test>]
let safeRings1 () =
    let r = Common.Level.safeRings 5 4 3
    Assert.AreEqual([4;0;1],r)
    
    let r2 = Common.Level.safeRings 5 4 2
    Assert.AreEqual([4;0],r2)
    
    let r3 = Common.Level.safeRings 5 4 4
    Assert.AreEqual([4;0;1;2],r3)
    
    let r4 = Common.Level.safeRings 5 3 5
    Assert.AreEqual([3;4;0;1;2],r4)
    

[<Test>]
let fakeRandomSeq1 () =
    let f = Common.Level.fakeRandomSeq [3;4;5]
    Assert.AreEqual(3, f 1)
    Assert.AreEqual(4, f 2)
    Assert.AreEqual(5, f 3)
    Assert.AreEqual(3, f 4)
    Assert.AreEqual(4, f 5)


[<Test>]
let randomIdx1 () =
    let random = Common.Level.fakeRandomSeq [3;7;5]
    let r = Common.Level.randomIdx [10;8;7] 3 random
    let e = [
        [3;4;5] |>Seq.ofList
        [7;0;1] |>Seq.ofList
        [5;6;0] |>Seq.ofList
    ]
    Assert.AreEqual(e, r)
    
[<Test>]
let randomSpin () =
    let level = [
        [|1;2;3;4|]
        [|5;6;7;8;9|]
        [|10;11;12|]
    ]
    let random = Common.Level.fakeRandomSeq [1;4;0]
    let r = Common.Level.randomSpin 2 level random
    let e = [|
        [|2;3|]
        [|9;5|]
        [|10;11|]
    |]
    Assert.AreEqual(e, r)
   
let ss = [|[|2;4;7|];[|3;5;2|];[|9;4;0|];[|4;9;2|];[|3;4;1|]|]
let pl1 = [|4;5;4;9;4|]
let pl2 = [|2;3;9;4;3|]
let pl3 = [|7;2;0;2;1|]
let pl4 = [|2;5;0;9;3|]
let pl5 = [|7;5;9;9;1|]
let pl6 = [|2;3;4;4;3|]
let pl7 = [|7;2;4;2;1|]
let pl8 = [|4;2;0;2;4|]
let pl9 = [|4;3;9;4;4|]

[<Test>]
let payLine () =
    let pl = Common.Line.onePayLine ss
    Assert.AreEqual(pl1, pl Game.Line.l1)
    Assert.AreEqual(pl2, pl Game.Line.l2)
    Assert.AreEqual(pl3, pl Game.Line.l3)
    Assert.AreEqual(pl4, pl Game.Line.l4)
    Assert.AreEqual(pl5, pl Game.Line.l5)
    Assert.AreEqual(pl6, pl Game.Line.l6)
    Assert.AreEqual(pl7, pl Game.Line.l7)
    Assert.AreEqual(pl8, pl Game.Line.l8)
    Assert.AreEqual(pl9, pl Game.Line.l9)
    
   
[<Test>]
let payLines () =
        let r = Common.Line.payLines Game.Line.allLines ss
        let er = [|pl1;pl2;pl3;pl4;pl5;pl6;pl7;pl8;pl9|]
        Assert.AreEqual(er, r)

let equal i =  fun j-> i=j 


[<Test>]
let countLineOnce () =
    let cf = Common.Line.countLineOnce (equal 3)
    
    let r1 = cf [|2;3;4|]
    Assert.AreEqual(Some(2,2,true),r1)
    
    let r2 = cf [||]
    Assert.AreEqual(None,r2)
    
    let r3 = cf [|3;3;3|]
    Assert.AreEqual(None,r3)
    
    let r4 = cf [|4;3;4;5;4|]
    Assert.AreEqual(Some(4,3,true), r4)
    
    let r4 = cf [|4;3;3;3;4|]
    Assert.AreEqual(Some(4,5,true), r4)
    
    let r5 = cf [|4;5;3;3;4|]
    Assert.AreEqual(Some(4,1,false), r5)
    
    
[<Test>]
let countLineTwice () =
    let cf = Common.Line.countLineTwice(equal 3)
    
    let r1 = cf [|2;3;4|]
    Assert.AreEqual((Some(2,2,true),Some(4,2,true)),r1)
    
    let r2 = cf [||]
    Assert.AreEqual((None,None),r2)
    
    let r3 = cf [|3;3;3|]
    Assert.AreEqual((None,None),r3)
    
    let r4 = cf [|4;3;4;5;4|]
    Assert.AreEqual((Some(4,3,true),Some(4,1,false)), r4)
    
    let r4 = cf [|4;3;3;3;4|]
    Assert.AreEqual((Some(4,5,true),None), r4)
    
    let r5 = cf [|4;5;3;3;4|]
    Assert.AreEqual((Some(4,1,false),Some(4,3,true)), r5)
    
    
[<Test>]
let countAllLineTwice () =
    let cf = Common.Line.countAllLineTwice(equal 4)
    let lines = [|
            [|2;3;9;4;3|]
            [|7;2;0;2;1|]
    |]
    let er = [|
        Some(2,1,false),Some(3,2,true)
        Some(7,1,false),Some(1,1,false)
    |]
    
    let rr = cf lines
    Assert.AreEqual(er, rr)
    
    
[<Test>]
let countSymbolTest () =
   let count = Common.Line.countSymbol (equal 2)
   Assert.AreEqual(1,  count [2;4;7])
   Assert.AreEqual(2,  count [2;2;7])
   Assert.AreEqual(3,  count [2;2;2])
   Assert.AreEqual(0,  count [0;1;3])
   Assert.AreEqual(0,  count [])

[<Test>]
let scanTest1 () =
   let cs = Common.Line.countSymbol (equal 2)
   let cw = Common.Line.countSymbol (equal 0)
   let r = Common.Line.scanScatter ss cs cw   
   let expected = Some(4, true)
   Assert.AreEqual(expected, r)
   
[<Test>]
let scanTest2 () =
   let cs = Common.Line.countSymbol (equal 2)
   let cw = Common.Line.countSymbol (equal 5)
   let r = Common.Line.scanScatter ss cs cw   
   let expected = Some(3, true)
   Assert.AreEqual(expected, r)
   
[<Test>]
let countTest1 () =
   let r = Common.Line.countScatter ss  (equal 4) (equal 5)
   let expected = Some(5, true),Some(5, true)
   Assert.AreEqual(expected, r)
   
[<Test>]
let countTest2 () =
   let r = Common.Line.countScatter ss  (equal 5) (equal 4)
   let expected = None, None
   Assert.AreEqual(expected, r)
 

[<Test>]
let countTest3 () =
   let r = Common.Line.countScatter ss  (equal 4) (equal 9)
   let expected = Some(1,false), Some(5,true)
   Assert.AreEqual(expected, r)
  
   
[<Test>]
let countTest4 () =
   let r = Common.Line.countScatter ss  (equal 3) (equal 9)
   let expected = None, Some(4,true)
   Assert.AreEqual(expected, r)