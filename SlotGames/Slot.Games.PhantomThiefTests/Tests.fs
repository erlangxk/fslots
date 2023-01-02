module Tests

open FSharp.Collections.ParallelSeq
open System.Collections.Generic
open System.Diagnostics
open Slot.Game.Prelude
open Xunit
open Slot.Games.PhantomThief
open System.Linq

let snapshot =
    [| [| 2; 4; 7 |]; [| 3; 5; 2 |]; [| 9; 4; 0 |]; [| 4; 9; 2 |]; [| 3; 4; 1 |] |]

let ss  = Core.lineup snapshot

[<Fact>]
let ``count gems 1`` () =
    let r = Common.countGems [ 3; 4 ] ss
    let er = Map [ 3, [ (4, 0); (1, 0) ]; 4, [ (4, 1); (3, 0); (2, 1); (0, 1) ] ]
    Assert.Equal<Map<int, list<int * int>>>(er, r)


[<Fact>]
let ``count gems 2`` () =
    let r = Common.countGems [ 2; 7 ] ss
    let er = Map [ 2, [ (3, 2); (1, 2); (0, 0) ]; 7, [ (0, 2) ] ]
    Assert.Equal<Map<int, list<int * int>>>(er, r)

[<Fact>]
let ``calc gems multiplier`` () =
    let payTable =
        Map[3,
            Map[(5, 5)
                (4, 4)
                (3, 3)
                (2, 2)
                (1, 1)]

            4,
            Map[(5, 10)
                (4, 20)
                (3, 30)
                (2, 40)
                (1, 50)]]

    let (tm, rs) = Common.countGemsResult [ 3; 4 ] payTable ss
    let ers = [ 4, [ (4, 1); (3, 0); (2, 1); (0, 1) ], 20; 3, [ (4, 0); (1, 0) ], 2 ]
    Assert.Equal(22, tm)
    Assert.Equal<Common.GemWinResult<int>>(ers, rs)

[<Fact>]
let ``calc gems multiplier found no multiplier`` () =
    let payTable =
        Map[3,
            Map[(5, 5)
                (4, 4)
                (3, 3)
                (1, 1)]

            4,
            Map[(5, 10)
                (3, 30)
                (2, 40)
                (1, 50)]]

    let (tm, rs) = Common.countGemsResult [ 3; 4 ] payTable ss
    let ers = []
    Assert.Equal(0, tm)
    Assert.Equal<Common.GemWinResult<int>>(ers, rs)

[<Fact>]
let ``count bonus`` () =
    let r0 = Common.countBonus (fun x -> x = 3) ss
    Assert.Equal(2, Seq.length r0)

    let r1 = Common.countBonus (fun x -> x = 4) ss
    Assert.Equal(4, Seq.length r1)


[<Fact>]
let ``concat gem idx `` () =
    let result = [ 4, [ (4, 1); (3, 0); (2, 1); (0, 1) ], 20; 3, [ (4, 0); (1, 0) ], 2 ]
    let r = Common.allGemsIdx result

    let er =
        seq {
            (4, 1)
            (3, 0)
            (2, 1)
            (0, 1)
            (4, 0)
            (1, 0)
        }

    Assert.Equal<seq<int * int>>(er, r)


[<Fact>]
let ``concat line idx`` () =
    let lineWin =
        [ 0, "a", 3, 1; 1, "b", 2, 0; 2, "c", 1, 4; 3, "e", 3, 2; 4, "f", 4, 3 ]

    let lines =
        [ [ 1; 1; 1; 1; 1 ]
          [ 0; 0; 0; 0; 0 ]
          [ 2; 2; 2; 2; 2 ]
          [ 0; 0; 1; 1; 1 ]
          [ 2; 2; 1; 1; 1 ] ]

    let lineMap = List.indexed lines |> Map.ofList
    let r = Common.allLineIdx lineMap lineWin

    let er =
        seq {
            (0, 1)
            (1, 1)
            (2, 1)
            (0, 0)
            (1, 0)
            (0, 2)
            (1, 2)
            (3, 1)
        }

    Assert.Equal<seq<int * int>>(er, r)

let fullCycleMainGame game reels lens =
     
   let folder state result  =
        let (totalFsOfSpin, totalSpinMul, totalSpin, totalFsOfCollapse,totalCollapseMul,totalCollapse, dict) = state
        let (freeSpin1, lineMul, freeSpin2, collapseMul, collapse) = result
        (totalFsOfSpin+freeSpin1, totalSpinMul+lineMul, totalSpin+1,
        totalFsOfCollapse+freeSpin2, totalCollapseMul+collapseMul, totalCollapse+collapse,
        Map.change collapse (fun x ->
            match x with
            | Some(c)-> Some(c+1)
            | None -> Some(1)
        ) dict)
   
   let idxMatrix starts = Test.genSlice starts lens Game.height
   
   let stopWatch = Stopwatch()  
   stopWatch.Start()
   
   let (totalFsOfSpin, totalSpinMul, totalSpin, totalFsOfCollapse,totalCollapseMul,totalCollapse, dict)=
                    Test.genStartIdx lens
                    |> PSeq.map idxMatrix
                    |> PSeq.map (MainGame.spinWithCollapse reels lens)
                    |> PSeq.fold folder (0,0,0,0,0,0,Map.empty)
                    
   stopWatch.Stop()
   let totalWin = totalCollapseMul + totalSpinMul
   let totalCost = totalSpin * Config.Line.totalLines
   printfn "*************************"
   printfn $"game:{game}"
   printfn $"time elapsed {stopWatch.Elapsed.TotalSeconds}"
   printfn ""
   
   printfn $"totalWin:{totalWin}"
   printfn $"totalSpin:{totalSpin}"
   printfn $"totalFreeSpin:{totalFsOfSpin + totalFsOfCollapse}"
   printfn ""
   printfn $"totalSpinMul:{totalSpinMul}"
   printfn $"totalCollapse:{totalCollapse}"
   printfn $"totalCollapseMul:{totalCollapseMul}"
   printfn ""
   printfn $"non Tum RTP  = {double totalSpinMul / double totalCost}"
   printfn $"Tum RTP  = {double totalCollapseMul / double totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  
   printfn $"@@@@collapse statistics@@@"
   for kv in dict  do
       printfn $"collapse {kv.Key}:{kv.Value}"
   printfn "@@@@@@@@@@@@@@@@@@@@@@@@@@"

let fullCycleFeatureGame game reels lens =
   let random = System.Random()
   let rng2  = random.NextDouble
   
   let idxMatrix starts = Test.genSlice starts lens Game.height

   let folder state result  =
        let (totalFsOfSpin, totalSpinLineMul, totalSpinGemsMul,totalSpin,
             totalFsOfCollapse,totalCollapseLineMul, totalCollapseGemsMul, totalCollapse,
             collapseDict) = state
        let  spinFs, spinLineMul, spinGemsMul, collapseFs, collapseLineMul, collapseGemsMul, collapseCount = result
        (totalFsOfSpin+spinFs, totalSpinLineMul+spinLineMul,totalSpinGemsMul+spinGemsMul, totalSpin+1,
        totalFsOfCollapse+collapseFs, totalCollapseLineMul+collapseLineMul, totalCollapseGemsMul+collapseGemsMul, totalCollapse+collapseCount,
        Map.change collapseCount (fun x ->
            match x with
            | Some(c)-> Some(c+1)
            | None -> Some(1)
        ) collapseDict)
   
   let stopWatch = Stopwatch()
   stopWatch.Start()
   
   let totalFsOfSpin, totalSpinLineMul, totalSpinGemsMul, totalSpin, totalFsOfCollapse,totalCollapseLineMul, totalCollapseGemsMul, totalCollapse, dict=
                    Test.genStartIdx lens
                    |> PSeq.map idxMatrix
                    |> PSeq.map (FeatureGame.spinWithCollapse reels lens rng2)
                    |> PSeq.fold folder (0,0,0,0,0,0,0,0,Map.empty)
                    
   stopWatch.Stop()
   
   let totalCost = totalSpin * Config.Line.totalLines
   let totalSpinGemsWin = totalSpinGemsMul * Config.Line.totalLines
   let totalCollapseGemsWin = totalCollapseGemsMul * Config.Line.totalLines
   let totalWin = totalCollapseLineMul + totalSpinLineMul + totalSpinGemsWin + totalCollapseGemsWin

   printfn $"game:{game}"
   printfn "*************************"
   printfn $"time elapsed {stopWatch.Elapsed.TotalSeconds}"
  
 
   printfn ""
   printfn $"totalSpin:{totalSpin}"
   printfn $"totalSpinLineMul:{totalSpinLineMul}"
   printfn $"totalSpinGemsWin:{totalSpinGemsWin}"
   printfn $"totalFreeOfSpin:{totalFsOfSpin}"

   printfn ""
   printfn $"totalCollapse:{totalCollapse}"
   printfn $"totalCollapseLineMul:{totalCollapseLineMul}"
   printfn $"totalCollapseGemsWin:{totalCollapseGemsWin}"
   printfn $"totalFreeOfCollapse:{totalFsOfCollapse}"

  
   printfn ""
   printfn $"non Tum LINE RTP  = {double totalSpinLineMul / double totalCost}"
   printfn $"non Tum Gems RTP  = {double totalSpinGemsWin / double totalCost}"
   printfn $"Tum LINE RTP  = {double totalCollapseLineMul / double totalCost}"
   printfn $"Tum Gems RTP  = {double totalCollapseGemsWin / double totalCost}"
   
   printfn $"totalWin:{totalWin}/totalCost:{totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  

   printfn $"@@@@collapse statistics@@@"
   for kv in dict  do
       printfn $"collapse {kv.Key}:{kv.Value}"
   printfn "@@@@@@@@@@@@@@@@@@@@@@@@@@" 
[<Fact>]
let ``test fully cycle of MainGameA`` () =
   let reels = Config.MainGame.MainA
   let lens = Config.MainGame.lensMainA
   fullCycleMainGame "MainGameA" reels lens
     
//[<Fact>]
let ``test fully cycle of MainGameB`` () =
   let reels = Config.MainGame.MainB
   let lens = Config.MainGame.lensMainB
   fullCycleMainGame "MainGameB" reels lens

[<Fact>]
let ``test fully cycle of FeatureGameA`` () =
   let reels = Config.FeatureGame.FeatureA
   let lens = Config.FeatureGame.lensFeatureA
   fullCycleFeatureGame "FeatureGameA" reels lens
   
//[<Fact>]
let ``test fully cycle of FeatureGameB`` () =
   let reels = Config.FeatureGame.FeatureB
   let lens = Config.FeatureGame.lensFeatureB
   fullCycleFeatureGame "FeatureGameB" reels lens      
                       
   
             
         
     
     