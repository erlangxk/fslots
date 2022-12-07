module Slot.Games.Tests.QueenBee.GameTests


open NUnit.Framework
open Slot.Games.QueenBee
open System.Linq
open FSharpPlus

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
  
    
   
[<Test>]
let testGenStartIdx2 () =
    let ms = [ for l in Game.Level.l1 -> l.Length ]
    Assert.AreEqual([43; 42; 40; 37; 34],ms)
    let m = Common.Rtp.genStartIdx ms |> Seq.take 35 |> Seq.last
    let er  = [0;0;0;1;0]
    Assert.AreEqual(er, m)
    
    
//[<Test>]
let testFullCombo() =
    let gameLevel = Game.Level.l3
    let ms = [ for l in gameLevel -> l.Length ]
    let startIdx = Common.Rtp.genStartIdx ms
    let lines =9
    let mutable totalWin:double = 0.0
    let mutable totalBet:double = 1.0
    for idx in startIdx do
        let slices = Common.Rtp.genSlice idx ms 3
        let r =  Common.Level.shoot gameLevel slices
        let result = Game.Core.snapshotResult r
        let m = result.plain.multiplier + result.scatter.multiplier * lines
        totalWin <- (double)m + totalWin
        totalBet <- (double)lines + totalBet
     
    printf $"full combo's RTP = {totalWin/totalBet}"     
      
                  
let random = System.Random()

//[<Ignore("rtp")>]
[<Test>]
let randomSpinLevel1() =
    let rng i =  random.Next(0,i)
    let lines =9
    let times = 10000
    let mutable total:double = 0.0
    for i in 1..times do
        let r = Game.Core.randomSpinLevel1 rng
        let m = r.plain.multiplier + r.scatter.multiplier * lines
        total <- total + (double)m
    let amount =times * lines
    printf $"RTP = {total/(double)amount}"  