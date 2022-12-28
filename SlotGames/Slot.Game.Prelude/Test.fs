namespace Slot.Game.Prelude

module Test =
    let fakeRandomSeq (numbers:seq<int>) =
         let state = numbers.GetEnumerator()
         let rec f (i:int) =
             if state.MoveNext() then
                 state.Current
             else
                 state.Reset()
                 f i
         f
    let rec genStartIdx (lens:list<int>) =    
        match lens with 
        | [] -> Seq.ofList([[]])
        | h::t ->  
            seq { for i in 0..h-1 do
                    for l in (genStartIdx t) do yield i::l }
              
    let genSlice(starts:list<int>)(lens: list<int>)(size:int) =
        List.map2 (fun s l-> Core.idxRing l s size) starts lens