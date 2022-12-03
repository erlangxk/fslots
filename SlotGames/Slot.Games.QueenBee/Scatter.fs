module Slot.Games.QueenBee.Scatter

let count (symbol: int) (source: seq<int>) =
    source
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


let countScatter (ss: int [] []) (scatter:int) (wild: int) =
    let cs = count(scatter)
    let cw = count(wild)
    let rl = scanScatter ss cs cw
    let rr = scanScatter (Array.rev ss) cs cw
    rl,rr