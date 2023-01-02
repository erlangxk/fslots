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
   
   let rec loopCollapse collapse freeSpin mul oldIdxMatrix oldLineResult oldBonus =
      let r = MainGame.collapse oldIdxMatrix oldLineResult oldBonus reels lens
      let idxMatrix2, _, mul2, lineResult2, bonus2 = r
      let fs = MainGame.freeSpin bonus2.Length
      if(mul2>0 || fs >0) then
          loopCollapse (collapse+1) (freeSpin+fs) (mul+mul2) idxMatrix2 lineResult2 bonus2
      else
          collapse,freeSpin,mul
   
   let spinOnce idx =
     let slices = Test.genSlice idx lens Game.height
     let idxMatrix, _, mul, lineResult, bonus =  MainGame.shoot reels slices
     let freeSpin1 = MainGame.freeSpin bonus.Length
     let (collapse,fs2,collapseMul) =
          if(mul >0 || freeSpin1 >0 ) then 
            loopCollapse 0 0 0 idxMatrix lineResult bonus
          else (0, 0, 0)
     freeSpin1, mul, fs2, collapseMul, collapse
     
   
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
        

   let stopWatch = Stopwatch()
   
   stopWatch.Start()
   
   let (totalFsOfSpin, totalSpinMul, totalSpin, totalFsOfCollapse,totalCollapseMul,totalCollapse, dict)=
                    Test.genStartIdx lens
                    |> PSeq.map spinOnce
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
   printfn $"totalSpinMul:{totalCollapseMul}"
   printfn ""
   printfn $"non Tum RTP  = {double totalSpinMul / double totalCost}"
   printfn $"Tum RTP  = {double totalCollapseMul / double totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  
   printfn $"@@@@collapse statistics@@@"
   for kv in dict  do
       printfn $"collapse {kv.Key}:{kv.Value}"
   printfn "@@@@@@@@@@@@@@@@@@@@@@@@@@"

let fullCycleFeatureGame game reels lens =
   let stopWatch = Stopwatch()
   let random = System.Random()
   let mutable totalFreeOfSpin = 0
   let mutable totalFreeOfCollapse  =0
   
   let mutable spin = 0
   let mutable totalSpinLineMul =0
   let mutable totalSpinGemsMul = 0
   
   let mutable totalCollapseLineMul = 0
   let mutable totalCollapseGemsMul = 0
   
   let mutable collapse = 0
   
   let dict = Dictionary<int,int>()
   
   let rng2  = random.NextDouble 
   
   let spinOnce idx =
     spin <- spin + 1
     let slices = Test.genSlice idx lens Game.height
     let idxMatrix, _, lineMul, lineResult, gemsMul, gemsResult, bonus =  FeatureGame.shoot reels slices
     
     let mul = lineMul + gemsMul * Config.Line.totalLines
     let freeSpin = FeatureGame.freeSpin bonus.Length rng2
     totalFreeOfSpin <- totalFreeOfSpin + freeSpin
     totalSpinLineMul <- totalSpinLineMul + lineMul
     totalSpinGemsMul <- totalSpinGemsMul + gemsMul * Config.Line.totalLines
     
     if(mul >0 || freeSpin > 0 ) then 
         let mutable run = true
         let mutable runningIdxMatrix = idxMatrix
         let mutable runningLineResult = lineResult
         let mutable runningGemsResult = gemsResult
         let mutable runningBonus = bonus
         let mutable loop = 1
         while run do   
            collapse <- collapse + 1
            let r = FeatureGame.collapse runningIdxMatrix runningLineResult runningGemsResult runningBonus reels lens
            let idxMatrix, _, lineMul, lineResult, gemsMul, gemsResult, bonus = r
            let mul = lineMul + gemsMul * Config.Line.totalLines
            let freeSpin = FeatureGame.freeSpin bonus.Length rng2
            totalFreeOfCollapse <- totalFreeOfCollapse + freeSpin
            totalCollapseLineMul <- totalCollapseLineMul + lineMul
            totalCollapseGemsMul <- totalCollapseGemsMul + gemsMul * Config.Line.totalLines

            if mul > 0 || freeSpin>0 then
                loop <- loop + 1
                runningIdxMatrix <- idxMatrix
                runningLineResult <- lineResult
                runningGemsResult <- gemsResult
                runningBonus <- bonus
            else
                let count = dict.GetValueOrDefault(loop)
                dict[loop]<-(count+1)
                run <- false
   
   stopWatch.Start()
   let startIdx = Test.genStartIdx lens
   startIdx |> Seq.iter spinOnce
   stopWatch.Stop()
   printfn "*************************"
   let ts = stopWatch.Elapsed
   printfn $"@@@@collapse statistics@@@"
   for kv in dict  do
       printfn $"collapse {kv.Key}:{kv.Value}"
   printfn "@@@@@@@@@@@@@@@@@@@@@@@@@@" 
   printfn $"time elapsed {ts.TotalSeconds}"
   printfn $"game:{game}"
  
   printfn ""
   printfn $"totalSpin:{spin}"
   printfn $"totalSpinLineMul:{totalSpinLineMul}"
   printfn $"totalSpinGemsMul:{totalSpinGemsMul}"
   printfn $"totalFreeOfSpin:{totalFreeOfSpin}"

   printfn ""
   printfn $"totalCollapse:{collapse}"
   printfn $"totalCollapseLineMul:{totalCollapseLineMul}"
   printfn $"totalCollapseGemsMul:{totalCollapseGemsMul}"
   printfn $"totalFreeOfCollapse:{totalFreeOfCollapse}"

   let totalCost = spin * Config.Line.totalLines
   let totalWin = totalCollapseLineMul  + totalCollapseGemsMul + totalSpinLineMul+ totalSpinGemsMul

   printfn ""
   printfn $"non Tum LINE RTP  = {double totalSpinLineMul / double totalCost}"
   printfn $"non Tum Gems RTP  = {double totalSpinGemsMul / double totalCost}"
   printfn $"Tum LINE RTP  = {double totalCollapseLineMul / double totalCost}"
   printfn $"Tum Gems RTP  = {double totalCollapseGemsMul / double totalCost}"
   
   printfn $"totalWin:{totalWin}/totalCost:{totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  


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

//[<Fact>]
let ``test fully cycle of FeatureGameA`` () =
   let reels = Config.FeatureGame.FeatureA
   let lens = Config.FeatureGame.lensFeatureA
   fullCycleFeatureGame "FeatureGameA" reels lens
   
//[<Fact>]
let ``test fully cycle of FeatureGameB`` () =
   let reels = Config.FeatureGame.FeatureB
   let lens = Config.FeatureGame.lensFeatureB
   fullCycleFeatureGame "FeatureGameB" reels lens      
                       
   
             
         
     
     