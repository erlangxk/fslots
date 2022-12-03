module Slot.Games.Tests.QueenBee.ScatterTests

open NUnit.Framework
open Slot.Games.QueenBee

let ss = [|[|2;4;7|];[|3;5;2|];[|9;4;0|];[|4;9;2|];[|3;4;1|]|]

[<Test>]
let countTest () =
   Assert.AreEqual(1,  Scatter.count 2  [2;4;7])
   Assert.AreEqual(2,  Scatter.count 2  [2;2;7])
   Assert.AreEqual(3,  Scatter.count 2  [2;2;2])
   Assert.AreEqual(0,  Scatter.count 2  [0;1;3])
   Assert.AreEqual(0,  Scatter.count 2  [])
   
[<Test>]
let scanTest1 () =
   let cs = Scatter.count 2
   let cw = Scatter.count 0
   let r = Scatter.scanScatter ss cs cw   
   let expected = Some(4, true)
   Assert.AreEqual(expected, r)

let scanTest2 () =
   let cs = Scatter.count 2
   let cw = Scatter.count 7
   let r = Scatter.scanScatter ss cs cw   
   let expected = Some(4, true)
   Assert.AreEqual(expected, r)