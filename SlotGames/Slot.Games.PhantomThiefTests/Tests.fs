module Tests

open System
open System.Diagnostics
open Slot.Game.Prelude
open Xunit
open Slot.Games.PhantomThief

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
   let stopWatch = Stopwatch()
   let mutable totalFreeSpin = 0
   let mutable totalSpinMul = 0
   let mutable totalCollapseMul = 0
   let mutable spin = 0
   let mutable collapse = 0
   
   let spinOnce idx =
     spin <- spin + 1
     let slices = Test.genSlice idx lens Game.height
     let idxMatrix, _, mul, lineResult, bonus =  MainGame.shoot reels slices
     let freeSpin = MainGame.freeSpin bonus.Length
     totalFreeSpin <- totalFreeSpin + freeSpin
     totalSpinMul <- totalSpinMul + mul

     if(mul >0 || freeSpin >0 ) then 
         let mutable run = true
         let mutable runningIdxMatrix = idxMatrix
         let mutable runningLineResult = lineResult
         let mutable runningBonus = bonus
       
         while run do
            collapse <- collapse + 1
            let r = MainGame.collapse runningIdxMatrix runningLineResult runningBonus reels lens
            let idxMatrix, _, mul, lineResult, bonus = r
            let freeSpin = MainGame.freeSpin bonus.Length
            totalFreeSpin <- totalFreeSpin + freeSpin
            totalCollapseMul <- totalCollapseMul + mul
            if mul > 0 || freeSpin > 0 then  
                runningIdxMatrix <- idxMatrix
                runningLineResult <- lineResult
                runningBonus <- bonus
            else
                run <- false
   stopWatch.Start()
   let startIdx = Test.genStartIdx lens
   startIdx |> Seq.iter spinOnce
   stopWatch.Stop()
   let totalWin = totalCollapseMul + totalSpinMul
   let totalCost = spin * Config.Line.totalLines
   printfn "*************************"
   let ts = stopWatch.Elapsed
   printfn $"time elapsed {ts.TotalSeconds}"
   printfn $"game:{game}"
   printfn $"totalWin:{totalWin}"
   printfn $"totalSpin:{spin}"
   printfn $"totalSpinMul:{totalSpinMul}"
   printfn $"totalCollapse:{collapse}"
   printfn $"totalSpinMul:{totalCollapseMul}"
   printfn $"totalFreeSpin:{totalFreeSpin}"
   printfn ""
   printfn $"non Tum RTP  = {double totalSpinMul / double totalCost}"
   printfn $"Tum RTP  = {double totalCollapseMul / double totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  


let fullCycleFeatureGame game reels lens =
   let stopWatch = Stopwatch()
   let random = System.Random()
   let mutable totalFreeSpin = 0
   let mutable totalLineMul =0
   let mutable totalGemsMul = 0
   let mutable totalCollapseLineMul = 0
   let mutable totalCollapseGemsMul = 0
   let mutable spin = 0
   let mutable collapse = 0
   
   let rng2  = random.NextDouble 
   
   let spinOnce idx =
     spin <- spin + 1
     let slices = Test.genSlice idx lens Game.height
     let idxMatrix, _, lineMul, lineResult, gemsMul, gemsResult, bonus =  FeatureGame.shoot reels slices
     
     let mul = lineMul + gemsMul * Config.Line.totalLines
     let freeSpin = FeatureGame.freeSpin bonus.Length rng2
     totalFreeSpin <- totalFreeSpin + freeSpin
     totalLineMul <- totalLineMul + lineMul
     totalGemsMul <- totalGemsMul + gemsMul * Config.Line.totalLines
     
     if(mul >0 || freeSpin > 0 ) then 
         let mutable run = true
         let mutable runningIdxMatrix = idxMatrix
         let mutable runningLineResult = lineResult
         let mutable runningGemsResult = gemsResult
         let mutable runningBonus = bonus
       
         while run do
            collapse <- collapse + 1
            let r = FeatureGame.collapse runningIdxMatrix runningLineResult runningGemsResult runningBonus reels lens
            let idxMatrix, _, lineMul, lineResult, gemsMul, gemsResult, bonus = r
            
            let mul = lineMul + gemsMul * Config.Line.totalLines
            let freeSpin = FeatureGame.freeSpin bonus.Length rng2
            totalFreeSpin <- totalFreeSpin + freeSpin
            totalCollapseLineMul <- totalCollapseLineMul + lineMul
            totalCollapseGemsMul <- totalCollapseGemsMul + gemsMul * Config.Line.totalLines

            if mul > 0 || freeSpin > 0 then  
                runningIdxMatrix <- idxMatrix
                runningLineResult <- lineResult
                runningBonus <- bonus
                runningGemsResult <- gemsResult
            else
                run <- false
   
   stopWatch.Start()
   let startIdx = Test.genStartIdx lens
   startIdx |> Seq.iter spinOnce
   stopWatch.Stop()
   let totalWin = totalCollapseLineMul + totalLineMul + totalCollapseGemsMul + totalGemsMul
   let totalCost = spin * Config.Line.totalLines
   printfn "*************************"
   let ts = stopWatch.Elapsed
   printfn $"time elapsed {ts.TotalSeconds}"
   printfn $"game:{game}"
   printfn $"totalWin:{totalWin}"
   printfn $"totalFreeSpin:{totalFreeSpin}"

   printfn ""
   printfn $"totalSpin:{spin}"
   printfn $"totalLineMul:{totalLineMul}"
   printfn $"totalGemsMul:{totalGemsMul}"
   printfn ""
   printfn $"totalCollapse:{collapse}"
   printfn $"totalCollapseLineMul:{totalCollapseLineMul}"
   printfn $"totalCollapseGemsMul:{totalCollapseGemsMul}"
   
   printfn ""
   printfn $"non Tum LINE RTP  = {double totalLineMul / double totalCost}"
   printfn $"non Tum Gems RTP  = {double totalGemsMul / double totalCost}"
   printfn $"Tum LINE RTP  = {double totalCollapseLineMul / double totalCost}"
   printfn $"Tum Gems RTP  = {double totalCollapseGemsMul / double totalCost}"
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
                       
   
             
         
     
     