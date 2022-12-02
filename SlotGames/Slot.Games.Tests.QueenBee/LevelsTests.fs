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
     
      
     