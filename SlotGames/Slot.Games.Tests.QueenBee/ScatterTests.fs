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

[<Test>]
let scanTest2 () =
   let cs = Scatter.count 2
   let cw = Scatter.count 5
   let r = Scatter.scanScatter ss cs cw   
   let expected = Some(3, true)
   Assert.AreEqual(expected, r)
   
[<Test>]
let countTest1 () =
   let r = Scatter.countScatter ss 4 5   
   let expected = Some(5, true),Some(5, true)
   Assert.AreEqual(expected, r)

[<Test>]
let countTest2 () =
   let r = Scatter.countScatter ss 5 4   
   let expected = None, None
   Assert.AreEqual(expected, r)

[<Test>]
let countTest3 () =
   let r = Scatter.countScatter ss 4 9   
   let expected = Some(1,false), Some(5,true)
   Assert.AreEqual(expected, r)
   
[<Test>]
let countTest4 () =
   let r = Scatter.countScatter ss 3 9
   let expected = None, Some(4,true)
   Assert.AreEqual(expected, r)