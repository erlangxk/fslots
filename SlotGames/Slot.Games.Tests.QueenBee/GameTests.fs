module Slot.Games.Tests.QueenBee.GameTests


open Xunit
open Slot.Games.QueenBee
open FSharpPlus
open FSharp.Collections.ParallelSeq

[<Fact>]
let testSpin1 () =
    let rng = Common.Test.fakeRandomSeq [3;4;5;6;7]
    let ss  = Game.Level.spinLevel1 rng
    let er = [|[|2; 4; 7|]; [|3; 5; 2|]; [|9; 4; 0|]; [|4; 9; 2|]; [|3; 4; 1|]|]
    Assert.Equal<int[][]>(er, ss)
    
[<Fact>]
let testLineOfSymbols () =
    let rng = Common.Test.fakeRandomSeq [3;4;5;6;7]
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
    
    Assert.Equal<int[][]>(er, linesOfSymbol |>> Seq.toArray |>Seq.toArray)
    
[<Fact>]
let testCalPlainWin () =
    let ss  = [|[|7; 8; 3|]; [|0; 7; 2|]; [|0; 2; 6|]; [|4; 1; 7|]; [|4; 9; 7|]|]
    let r,_ = Game.Core.queenBeeComputeLineResult ss
    Assert.Equal(15, r)
  
[<Fact>]
let testGenStartIdx2 () =
    let ms = [ for l in Game.Level.l1 -> l.Length ]
    Assert.Equal<list<int>>([43; 42; 40; 37; 34],ms)
    let m = Common.Test.genStartIdx ms |> Seq.take 35 |> Seq.last
    let er  = [0;0;0;1;0]
    Assert.Equal<list<int>>(er, m)
 
let spinOnce lens gameLevel idx =
     let slices = Common.Test.genSlice idx lens Game.Level.height
     let r =  Common.Level.shoot gameLevel slices
     let result = Game.Core.computeResult r
     result.totalMul
     
[<Fact>]
let testMainCycle() =
    let gameLevel = Game.Level.l1
    let lens = [ for l in gameLevel -> l.Length ]
    let totalCycle = lens |> List.fold (fun s i -> s * i) 1
    let oneSpin = spinOnce lens gameLevel
    let startIdx = Common.Test.genStartIdx lens
    let totalWin = startIdx
                |> PSeq.map oneSpin
                |> PSeq.sum
    printfn $"totalWin:{totalWin}"
    printfn $"totalCycle:{totalCycle}"
    printfn $"full combo's RTP = {double totalWin / double (totalCycle * 9)}"     
      
                  
let rng  =
    let random = System.Random()
    fun i -> random.Next(0,i)

//[<Ignore("rtp")>]
[<Fact>]
let randomSpinLevel1() =
    let times = 100
    let mutable total:double = 0.0
    for i in 1..times do
        let r = Game.Core.randomSpinLevel1 rng
        total <- total + double r.totalMul
    let amount =times * Game.Line.totalLines
    printfn $"RTP = {total / double amount}"  