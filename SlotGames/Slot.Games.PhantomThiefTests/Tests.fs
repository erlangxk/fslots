module Tests

open System
open Xunit
open Slot.Games.PhantomThief

let ss =
        [| [| 2; 4; 7 |]; [| 3; 5; 2 |]; [| 9; 4; 0 |]; [| 4; 9; 2 |]; [| 3; 4; 1 |] |]

[<Fact>]
let ``count gems`` () =
    let r = Common.countGems [ 3; 4 ] ss
    let er = Map [ 3, [ (4, 0); (1, 0) ]; 4, [ (4, 1); (3, 0); (2, 1); (0, 1) ] ]
    Assert.Equal<Map<int, list<int * int>>>(er, r)

[<Fact>]
let ``count bonus`` () =
    let r = Common.countBonus (fun x-> x=3) ss
    Assert.Equal(2,r)
    
    let r = Common.countBonus (fun x-> x=4) ss
    Assert.Equal(2,r)