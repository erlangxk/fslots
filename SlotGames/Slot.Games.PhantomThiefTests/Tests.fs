module Tests

open System
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
    let r = Common.countBonus (fun x -> x = 3) ss
    Assert.Equal(2, r)

    let r = Common.countBonus (fun x -> x = 4) ss
    Assert.Equal(4, r)


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
