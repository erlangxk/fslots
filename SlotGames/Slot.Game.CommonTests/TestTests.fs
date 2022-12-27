module TestTests

open Xunit
open Slot.Game.Common

[<Fact>]
let fakeRandomSeq1 () =
    let f = Test.fakeRandomSeq [3;4;5]
    Assert.Equal(3, f 1)
    Assert.Equal(4, f 2)
    Assert.Equal(5, f 3)
    Assert.Equal(3, f 4)
    Assert.Equal(4, f 5)

[<Fact>]
let genStartIdx () =
    let r = Test.genStartIdx [3;3]
    let er = [[0; 0]; [0; 1]; [0; 2]; [1; 0]; [1; 1]; [1; 2]; [2; 0]; [2; 1]; [2; 2]]
    Assert.Equal(er,r)
    Assert.Equal(9, Seq.length r)
    
    let r2 = Test.genStartIdx [5;3;4]
    Assert.Equal(5*3*4, Seq.length r2)
    
[<Fact>]
let genSlice () =
    let r = Test.genSlice [3;4;5] [6;5;7] 3
    let s1 = [3;4;5]
    let s2 = [4;0;1]
    let s3 = [5;6;0]
    let er = [s1;s2;s3]
    Assert.Equal<list<list<int>>>(er,r)