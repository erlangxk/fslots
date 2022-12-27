module CoreTests

open Slot.Game.Common
open Xunit
open System

[<Fact>]
let ``count consecutive symbols`` () =
    let cf = Core.countConsecutiveSymbols (fun x -> x = 3)
    Assert.Equal(Some(2, 2, true), cf [ 2; 3; 4 ])
    Assert.Equal(None, cf []) 
    Assert.Equal(None, cf [ 3; 3; 3 ])
    Assert.Equal(Some(4, 3, true), cf [ 4; 3; 4; 5; 4 ])
    Assert.Equal(Some(4, 5, true), cf [ 4; 3; 3; 3; 4 ])
    Assert.Equal(Some(4, 1, false), cf [ 4; 5; 3; 3; 4 ])

[<Fact>]
let ``check pick from one reel`` () =
 
    let reel = [|1;2;3;4;5;6;7;8|]
    let r1 = Core.cherryPick reel [3;5;7]
    Assert.Equal<list<int>>([4;6;8],r1)
   
    let r2 = Core.cherryPick reel [0;1;2]
    Assert.Equal<list<int>>([1;2;3],r2)   


[<Fact>]
let ``snapshot with list of list of index`` () =
    
    let reels =[
        [|10;20;30;40;50|]
        [|1;2;3;4;5;6;7;8|]
        [|5;4;3;2;1|] ]
    
    let idx = [
        [0;2;3]
        [1;4;5]
        [1;3;4]
    ]
    
    let r = Core.snapshot reels idx
    
    let er = [|
        [|10;30;40|]
        [|2;5;6|]
        [|4;2;1|]
    |]
    
    Assert.Equal<int[][]>(er, r)

[<Fact>]
let ``get some index starts from a stop`` () =
    let r = Core.idxRing 5 4 3
    Assert.Equal<list<int>>([4;0;1],r)
    
    let r2 = Core.idxRing 5 4 2
    Assert.Equal<list<int>>([4;0],r2)
    
    let r3 = Core.idxRing 5 4 4
    Assert.Equal<list<int>>([4;0;1;2],r3)
    
    let r4 = Core.idxRing 5 3 5
    Assert.Equal<list<int>>([3;4;0;1;2],r4)
    
    
[<Fact>]
let randomReelIdx () =
    let random = Test.fakeRandomSeq [3;7;5]
    let r = Core.randomReelIdx [10;8;7] 3 random
    let e = [
        [3;4;5]
        [7;0;1]
        [5;6;0]
    ]
    Assert.Equal<list<list<int>>>(e, r)
    
    
[<Fact>]
let randomSnapshot () =
    let level = [
        [|1;2;3;4|]
        [|5;6;7;8;9|]
        [|10;11;12|]
    ]
    let random = Test.fakeRandomSeq [1;4;0]
    let r = Core.randomSnapshot 2 level random
    let e = [|
        [|2;3|]
        [|9;5|]
        [|10;11|]
    |]
    Assert.Equal<int[][]>(e, r)
    
    
let ss = [|[|2;4;7|];[|3;5;2|];[|9;4;0|];[|4;9;2|];[|3;4;1|]|]
let pl1 = [4;5;4;9;4]
let pl2 = [2;3;9;4;3]
let pl3 = [7;2;0;2;1]
let pl4 = [2;5;0;9;3]
let pl5 = [7;5;9;9;1]
let pl6 = [2;3;4;4;3]
let pl7 = [7;2;4;2;1]
let pl8 = [4;2;0;2;4]
let pl9 = [4;3;9;4;4]

let l1 = [1; 1; 1; 1; 1]
let l2 = [0; 0; 0; 0; 0]
let l3 = [2; 2; 2; 2; 2]                     
let l4 = [0; 1; 2; 1; 0]
let l5 = [2; 1; 0; 1; 2]
let l6 = [0; 0; 1; 0; 0]
let l7 = [2; 2; 1; 2; 2]
let l8 = [1; 2; 2; 2; 1]
let l9 = [1; 0; 0; 0; 1]

[<Fact>]
let onePayLine () =
    let pl = Core.onePayLine ss
    Assert.Equal<list<int>>(pl1, pl l1)
    Assert.Equal<list<int>>(pl2, pl l2)
    Assert.Equal<list<int>>(pl3, pl l3)
    Assert.Equal<list<int>>(pl4, pl l4)
    Assert.Equal<list<int>>(pl5, pl l5)
    Assert.Equal<list<int>>(pl6, pl l6)
    Assert.Equal<list<int>>(pl7, pl l7)
    Assert.Equal<list<int>>(pl8, pl l8)
    Assert.Equal<list<int>>(pl9, pl l9)
    
   
[<Fact>]
let allPayLines () =
        let r = Core.allPayLines [l1;l2;l3;l4;l5;l6;l7;l8;l9] ss
        let er = [pl1;pl2;pl3;pl4;pl5;pl6;pl7;pl8;pl9]
        Assert.Equal<list<list<int>>>(er,r)
        
                
[<Fact>]
let checkLevel1 () =
    let level = [
        [|1;2;3;4|]
        [|5;6;7;8;9|]
        [|10;11;12|]
    ]
    Assert.Throws<ArgumentException>(
       fun ()-> Core.checkReels level 3 5 ) |> ignore
    Assert.Throws<ArgumentException>(
       fun ()-> Core.checkReels level 4 5 ) |> ignore
 
let pt1 =
    Map [ (5, 100)
          (4, 10)
          (3, 5)
          (2, 1) ]

let pt2 = Map[(9, pt1)]

[<Fact>]
let getMultiplier () =
    let lookup s = Core.getMultiplier s pt1
    let r1 = lookup 5
    Assert.Equal(Some(100), r1)
    let r2 = lookup 1
    Assert.Equal(None, r2)

[<Fact>]
let getNestedMultiplier () =
    let lookup s c = Core.getNestedMultiplier s c pt2

    let r1 = lookup 9 5
    Assert.Equal(Some(100), r1)
    let r1 = lookup 9 2
    Assert.Equal(Some(1), r1)
    let r1 = lookup 9 1
    Assert.Equal(None, r1)
    let r1 = lookup 12 2
    Assert.Equal(None, r1)