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
    