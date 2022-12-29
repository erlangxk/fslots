module GameTests

open Xunit
open Slot.Games.QueenBee
open FSharpPlus
open FSharp.Collections.ParallelSeq
open Slot.Game.Prelude

[<Fact>]
let testSpin1 () =
    let rng = Test.fakeRandomSeq [3;4;5;6;7]
    let ss  = Level.spinLevel1 rng
    let er = [|[|2; 4; 7|]; [|3; 5; 2|]; [|9; 4; 0|]; [|4; 9; 2|]; [|3; 4; 1|]|]
    Assert.Equal<int[][]>(er, ss)
    
[<Fact>]
let testLineOfSymbols () =
    let rng = Test.fakeRandomSeq [3;4;5;6;7]
    let ss  = Level.spinLevel1 rng
    let linesOfSymbol = Line.queenBeePayLines ss
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
    let r,_ = Pack.queenBeeComputeLineResult ss
    Assert.Equal(15, r)
  
[<Fact>]
let testGenStartIdx2 () =
    let ms = [ for l in Level.l1 -> l.Length ]
    Assert.Equal<list<int>>([43; 42; 40; 37; 34],ms)
    let m = Test.genStartIdx ms |> Seq.take 35 |> Seq.last
    let er  = [0;0;0;1;0]
    Assert.Equal<list<int>>(er, m)
 
let spinOnce lens gameLevel idx =
     let slices = Test.genSlice idx lens Level.height
     let r =  Core.snapshot gameLevel slices
     let result = Pack.computeResult r
     result.totalMul
     
//[<Fact>]
let testMainCycle() =
    let gameLevel = Level.l1
    let lens = Core.lens gameLevel
    let totalCycle = lens |> List.fold (fun s i -> s * i) 1
    let oneSpin = spinOnce lens gameLevel
    let startIdx = Test.genStartIdx lens
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
    let times = 1000000
    let mutable total:double = 0.0
    for i in 1..times do
        let r = Pack.randomSpinLevel1 rng
        total <- total + double r.totalMul
    let amount =times * Line.totalLines
    printfn $"RTP = {total / double amount}"  