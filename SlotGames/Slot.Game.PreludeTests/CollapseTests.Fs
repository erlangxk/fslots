module CollapseTests


open Slot.Game.Prelude
open Xunit

[<Fact>]
let ``test the removal of index matrix`` () = 
    
    let matrix = [
        [3;4;5]
        [23;31;40]
        [4;7;8]
        [6;9;10]
        [7;8;19]
    ]
    let lines = [
        [ 1; 1; 1; 1; 1 ],3
        [ 0; 0; 0; 0; 0 ],2
        [ 2; 2; 2; 2; 2 ],1
        [ 0; 0; 1; 1; 1 ],3
        [ 2; 2; 1; 1; 1 ],4
    ]
    
    let r = Collapse.labelIdxMatrix matrix lines
    
    let er = [
        []
        []
        [4;8]
        [6;10]
        [7;8;19]
    ]
    
    Assert.Equal<list<list<int>>>(er,r)
    
    
[<Fact>]
let ``test the reload for no load needed`` () =
    let l1 = [1;2;3]
    let l2 = [1;2;3]
    let r = Collapse.reloadReel l1 l2 7
    Assert.Equal<list<int>>(l2, r)


[<Fact>]
let ``test the reload for load 3`` () =
    let l1 = [1;2;3]
    let l2 = []
    let r = Collapse.reloadReel l1 l2 7
    Assert.Equal<list<int>>([5;6;0], r)
    

[<Fact>]
let ``test the reload for load 2`` () =
    let r = Collapse.reloadReel [1;2;3] [2] 7
    Assert.Equal<list<int>>([6;0;2], r)
    
    let r = Collapse.reloadReel [1;2;3] [1] 7
    Assert.Equal<list<int>>([6;0;1], r)
    
    let r = Collapse.reloadReel [1;2;3] [3] 7
    Assert.Equal<list<int>>([6;0;3], r)
    
     
[<Fact>]
let ``test the reload for load 1`` () =
    let r = Collapse.reloadReel [1;2;3] [2;3] 7
    Assert.Equal<list<int>>([0;2;3], r)
    
    let r = Collapse.reloadReel [1;2;3] [1;3] 7
    Assert.Equal<list<int>>([0;1;3], r)
    
    let r = Collapse.reloadReel [1;2;3] [1;2] 7
    Assert.Equal<list<int>>([0;1;2], r)
    
    
[<Fact>]
let ``test reload index matrix`` () =
    let om = [
        [3;4;5]
        [23;31;40]
        [4;7;8]
        [6;9;10]
        [7;8;19]
    ]
     
    let nm = [
        []
        []
        [4;8]
        [6;10]
        [7;8;19]
    ]
    
    let er = [
        [0;1;2]
        [20;21;22]
        [3;4;8]
        [5;6;10]
        [7;8;19]
    ]
    
    let r = Collapse.reloadIdxMatrix om nm [7;41;12;15;21]
    Assert.Equal<list<list<int>>>(er,r)
    
    