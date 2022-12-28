module CommonTests

open FSharpPlus
open Xunit
open Slot.Games.QueenBee
open Slot.Game.Prelude

let equal i =  fun j-> i=j 

[<Fact>]
let countLineTwice () =
    let cf w = Common.countLineTwice w (equal 3)
    
    let r1 = cf 3 [2;3;4]
    Assert.Equal((Some(2,2,true),Some(4,2,true)),r1)
    
    let r2 = cf 3 []
    Assert.Equal((None,None),r2)
    
    let r3 = cf 3 [3;3;3]
    Assert.Equal((None,None),r3)
    
    let r4 = cf 5 [4;3;4;5;4]
    Assert.Equal((Some(4,3,true),Some(4,1,false)), r4)
    
    let r4 = cf 5 [4;3;3;3;4]
    Assert.Equal((Some(4,5,true),None), r4)
    
    let r5 = cf 5 [4;5;3;3;4]
    Assert.Equal((Some(4,1,false),Some(4,3,true)), r5)
    
    
[<Fact>]
let countAllLineTwice () =
    let cf w = Common.countAllLineTwice w (equal 4)
    let lines = [
             [2;3;9;4;3]
             [7;2;0;2;1]
    ]
    let er = [
        Some(2,1,false),Some(3,2,true)
        Some(7,1,false),Some(1,1,false)
    ]
    
    let rr = cf 5 lines
    Assert.Equal<Common.LeftRightLineResult<int>>(er, rr)
    
    

let ss = [|[|2;4;7|];[|3;5;2|];[|9;4;0|];[|4;9;2|];[|3;4;1|]|]

[<Fact>]
let scanTest1 () =
   let cs = Core.countSymbol (equal 2)
   let cw = Core.countSymbol (equal 0)
   let r = Common.scanScatter ss cs cw   
   let expected = Some(4, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let scanTest2 () =
   let cs = Core.countSymbol (equal 2)
   let cw = Core.countSymbol (equal 5)
   let r = Common.scanScatter ss cs cw   
   let expected = Some(3, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let scanTest3 () =
   let cs = Core.countSymbol (equal 2)
   let cw = Core.countSymbol (equal 5)
   let r = Common.scanScatter Array.empty cs cw   
   let expected = None
   Assert.Equal(expected, r)

[<Fact>]
let scanTest4 () =
   let cs = Core.countSymbol (equal 10)
   let cw = Core.countSymbol (equal 11)
   let r = Common.scanScatter ss cs cw   
   let expected = None
   Assert.Equal(expected, r)
   
[<Fact>]
let countTest1 () =
   let r = Common.countScatter ss  (equal 4) (equal 5)
   let expected = Some(5, true),Some(5, true)
   Assert.Equal(expected, r)
   
[<Fact>]
let countTest2 () =
   let r = Common.countScatter ss  (equal 5) (equal 4)
   let expected = None, None
   Assert.Equal(expected, r)
 

[<Fact>]
let countTest3 () =
   let r = Common.countScatter ss  (equal 4) (equal 9)
   let expected = Some(1,false), Some(5,true)
   Assert.Equal(expected, r)
  
   
[<Fact>]
let countTest4 () =
   let r = Common.countScatter ss  (equal 3) (equal 9)
   let expected = None, Some(4,true)
   Assert.Equal(expected, r)
    