module Tests

open System
open Slot.Game.Prelude
open Xunit
open Slot.Games.PhantomThief

let ss =
    [| [| 2; 4; 7 |]; [| 3; 5; 2 |]; [| 9; 4; 0 |]; [| 4; 9; 2 |]; [| 3; 4; 1 |] |]

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
    //int * 'a * int * int
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
   let mutable totalFreeSpin = 0
   let mutable totalSpinMul = 0
   let mutable totalCollapseMul = 0
   let mutable spin = 0
   let mutable collapse = 0
   
   let spinOnce idx =
     spin <- spin + 1
     let slices = Test.genSlice idx lens Config.height
     let idxMatrix, _, mul, lineResult, bonus =  MainGame.shoot reels slices
     if bonus.Length =3 then totalFreeSpin <- totalFreeSpin + 6
     totalSpinMul <- totalSpinMul + mul

     if(mul >0 || bonus.Length >=3 ) then 
         let mutable run = true
         let mutable runningIdxMatrix = idxMatrix
         let mutable runningLineResult = lineResult
         let mutable runningBonus = bonus
       
         while run do
            collapse <- collapse + 1
            let r = MainGame.collapse runningIdxMatrix runningLineResult runningBonus reels lens
            let idxMatrix, _, mul, lineResult, bonus = r
            if bonus.Length =3 then totalFreeSpin <- totalFreeSpin + 6
            if mul > 0 || bonus.Length>=3 then  
                runningIdxMatrix <- idxMatrix
                runningLineResult <- lineResult
                runningBonus <- bonus
                totalCollapseMul <- totalCollapseMul + mul
            else
                run <- false
   let startIdx = Test.genStartIdx lens
   startIdx |> Seq.iter spinOnce 
   let totalWin = totalCollapseMul + totalSpinMul
   let totalCost = spin * 30
   printfn $"game:{game}"
   printfn $"totalWin:{totalWin}"
   printfn $"totalSpin:{spin}"
   printfn $"totalSpinMul:{totalSpinMul}"
   printfn $"totalCollapse:{collapse}"
   printfn $"totalSpinMul:{totalCollapseMul}"
   printfn $"totalFreeSpin:{totalFreeSpin}"
   
   printfn $"non Tum RTP  = {double totalSpinMul / double totalCost}"
   printfn $"Tum RTP  = {double totalCollapseMul / double totalCost}"
   printfn $"full cycle's RTP = {double totalWin / double totalCost}"  

[<Fact>]
let ``test fully cycle of MainGameA`` () =
   let reels = Config.MainGame.MainA
   let lens = Config.MainGame.lensMainA
   fullCycleMainGame "MainGameA" reels lens
     
      
[<Fact>]
let ``test fully cycle of MainGameB`` () =
   let reels = Config.MainGame.MainB
   let lens = Config.MainGame.lensMainB
   fullCycleMainGame "MainGameB" reels lens
     
      
                       
   
             
         
     
     