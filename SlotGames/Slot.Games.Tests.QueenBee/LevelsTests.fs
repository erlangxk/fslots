module Slot.Games.Tests.QueenBee.LevelsTests

open NUnit.Framework
open Slot.Games.QueenBee


[<Test>]
let TestSpin1 () =
    let result = Levels.queenBeeSpin Levels.l1 [3;4;5;6;7]
    Assert.IsFalse(result.IsNone)
    match result.Value with
        |[s1;s2;s3;s4;s5] ->
            Assert.AreEqual([2;4;7], s1)
            Assert.AreEqual([3;5;2], s2)
            Assert.AreEqual([9;4;0], s3)
            Assert.AreEqual([4;9;2], s4)
            Assert.AreEqual([3;4;1], s5)
        | _ -> Assert.Fail()


[<Test>]
let TestRing () =
     match Levels.rings 5 4 3  with
      | Some(r1) -> Assert.AreEqual(r1, [4;0;1])
      | None -> Assert.Fail()
      
     match Levels.rings 5 4 5  with
      | Some(r1) -> Assert.AreEqual(r1, [4;0;1;2;3])
      | None -> Assert.Fail()
      
     Assert.AreEqual(Levels.rings 5 4 0,None)
     Assert.AreEqual(Levels.rings 5 5 3, None)
     Assert.AreEqual(Levels.rings 5 -1 3, None)
     Assert.AreEqual(Levels.rings 5 4 6, None)
     
let cs = Scatter.count Symbols.ScatterId
let cw = Scatter.count Symbols.WildId

//check no more than 2 scatter and/or 2 wild in the spin result
let checkReel reel =
    let len = Array.length reel
    let mutable i =0
    while (i<len) do
        let ring = Levels.rings len i Levels.height
        i <- i+1
        Assert.IsFalse(ring.IsNone)
        let r = ring.Value
        let result = Levels.safeSpinOneLine  reel r
        let t = (cs result) + (cw result)
        Assert.IsTrue(t<2)

[<Test>]
let TestAllReels () =
    Assert.AreEqual(5, Levels.l1.Length)
    for r in Levels.l1 do
        checkReel r
    Assert.AreEqual(5, Levels.l2.Length)
    for r in Levels.l2 do
        checkReel r
    Assert.AreEqual(5, Levels.l3.Length)
    for r in Levels.l3 do
        checkReel r
 
// check there's no wild symbol in first reel and last reel  
[<Test>]
let TestWildInFirst2Last () =
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l1[0])
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l2[0])
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l3[0])
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l1[Levels.l1.Length-1])
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l2[Levels.l1.Length-1])
    Assert.IsFalse(Array.contains Symbols.WildId Levels.l3[Levels.l1.Length-1])

    