namespace Slot.Games.PhantomThief

open Slot.Game.Prelude

module Common =

    let randomFreeGame (r: decimal) =
        if r <= 0.3350m then 2
        elif r <= 0.6500m then 3
        else 4


    let countBonus<'a when 'a: equality> (isBonus: 'a -> bool) (snapshot: 'a[][]) =
        snapshot |> Seq.fold (fun count arr -> count + Core.countSymbol isBonus arr) 0

    type Pos = int * int

    let countGems<'a when 'a: comparison> (gems: list<'a>) (snapshot: 'a[][]) =
        let all =
            seq {
                for i in 0 .. snapshot.Length - 1 do
                    for j in 0 .. snapshot[i].Length - 1 do
                        (i, j, snapshot[i][j])
            }

        let folder (state: Map<'a, list<Pos>>) (i, j, e) =
            match Map.tryFind e state with
            | Some(l) -> state |> Map.add e ((i, j) :: l)
            | None -> state

        let init = gems |> List.map (fun g -> g, List.empty<Pos>) |> Map.ofList

        Seq.fold folder init all
