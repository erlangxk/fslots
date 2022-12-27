module ResultJsonTests


open Xunit
open Slot.Games.QueenBee.ResultJson
open Slot.Games.QueenBee.Calc

[<Fact>]
let testEncodeResult () =
    let result = {
        snapshot =  [|[|2; 4; 7|]; [|3; 5; 2|]; [|9; 4; 0|]; [|4; 9; 2|]; [|3; 4; 1|]|]
        scatterWin = []
        scatterMul = 0
        lineWin =[]
        lineMul = 0
    }
    let r = encodeResult result
    let er = """{"ss":[[2,4,7],[3,5,2],[9,4,0],[4,9,2],[3,4,1]],"sm":0,"lm":0}"""
    Assert.Equal(er, r)

