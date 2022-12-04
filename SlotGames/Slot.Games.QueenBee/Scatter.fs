module Slot.Games.QueenBee.Scatter

open System.Collections.Generic
open System.Linq

type Reel = int[]
let count (symbol: int) (reel: seq<int>) =
    reel
    |> Seq.fold (fun s t -> if t = symbol then s + 1 else s) 0
(*
the logic to count scatter has 2 options
a) count all scatter and wild in one reel, could be more than 1
b) one reel at most 1
the code below use option a.
*)
let scanScatter (ss: int [] []) (countScatter: seq<int> -> int) (countWild: seq<int> -> int) =
    let first = countScatter ss[0]    
    if first > 0 then
        let mutable consecutive = true
        let mutable total = first
        let mutable replace = false
        let mutable i = 1
        while (i < ss.Length && consecutive) do
            let reel = ss[i]
            let si = countScatter reel
            let wi = countWild reel
            let ti = si + wi
            if ti>0 then
                 total <- total + ti
            else
                consecutive <- false
            if wi > 0 then replace <- true
            i <- i + 1
        Some(total, replace)
    else
        None

 
let countScatter (snapshot: int [] []) (scatter:int) (wild: int) =
    if snapshot.Length > 1 then 
        let cs = count(scatter)
        let cw = count(wild)
        let rl = scanScatter snapshot cs cw
        let rr = scanScatter (Array.rev snapshot) cs cw
        rl,rr
    else invalidArg "snapshot" $"snapshot min width should be 2, not ${snapshot.Length}"
    
    
let scanScatter2 (ss: IEnumerator<Reel>) (countScatter: seq<int> -> int) (countWild: seq<int> -> int) =
    if ss.MoveNext() then 
        let first = countScatter ss.Current
        if first > 0 then
            let mutable consecutive = true
            let mutable total = first
            let mutable replace = false
            while (ss.MoveNext() && consecutive) do
                let reel = ss.Current
                let si = countScatter reel
                let wi = countWild reel
                let ti = si + wi
                if ti>0 then
                     total <- total + ti
                else
                    consecutive <- false
                if wi > 0 then replace <- true
            Some(total, replace)
        else
            None
    else None

let countScatter2 (snapshot: Reel[]) (scatter:int) (wild: int) =
    if snapshot.Length > 1 then
        let cs = count(scatter)
        let cw = count(wild)
        let el2r = snapshot.AsEnumerable().GetEnumerator()
        let rl = scanScatter2 el2r cs cw
        let er2l = snapshot.Reverse().GetEnumerator()
        let rr = scanScatter2 er2l cs cw
        rl,rr
    else invalidArg "snapshot" $"snapshot min width should be 2, not ${snapshot.Length}"
    