module Slot.Games.Tests.QueenBee.GameTests


open NUnit.Framework
open Slot.Games.QueenBee
open System.Linq

[<Test>]
let testSpin1 () =
    let rng = Common.Level.fakeRandomSeq [3;4;5;6;7]
    let ss  = Game.Level.spinLevel1 rng
    let er = [|[|2; 4; 7|]; [|3; 5; 2|]; [|9; 4; 0|]; [|4; 9; 2|]; [|3; 4; 1|]|]
    Assert.AreEqual(er, ss)
    
[<Test>]
let testLineOfSymbols () =
    let rng = Common.Level.fakeRandomSeq [3;4;5;6;7]
    let ss  = Game.Level.spinLevel1 rng
    let linesOfSymbol = Game.Line.queenBeePayLines ss
    let er = [|[|4; 5; 4; 9; 4|]
               [|2; 3; 9; 4; 3|]
               [|7; 2; 0; 2; 1|]
               [|2; 5; 0; 9; 3|];
               [|7; 5; 9; 9; 1|]
               [|2; 3; 4; 4; 3|]
               [|7; 2; 4; 2; 1|]
               [|4; 2; 0; 2; 4|]
               [|4; 3; 9; 4; 4|]|]
    
    Assert.AreEqual(er, linesOfSymbol)
    
[<Test>]
let testCalPlainWin () =
    let ss  = [|[|7; 8; 3|]; [|0; 7; 2|]; [|0; 2; 6|]; [|4; 1; 7|]; [|4; 9; 7|]|]
    let linesOfSymbol = Game.Line.queenBeePayLines ss
    let r = Game.Core.plainResult linesOfSymbol
    Assert.AreEqual(15, r.multiplier)
   
let random = System.Random()

[<Ignore("rtp")>]
let randomSpinLevel1() =
    let rng i =  random.Next(0,i)
    let lines =9
    let times = 10000000
    let mutable total:double = 0.0
    for i in 1..times do
        let r = Game.Core.randomSpinLevel1 rng
        let m = r.plain.multiplier + r.scatter.multiplier * lines
        total <- total + (double)m
    let amount =times * lines
    printf $"RTP = {total/(double)amount}"  