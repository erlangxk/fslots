namespace Slot.Games.PhantomThief

open Slot.Game.Prelude

module MainGame =

    let MainGameA = "MainGameA"
    let MainGameB = "MainGameB"

    let calcLineWin symbol count =
        Config.MainGame.linePayTable |> Core.getNestedMultiplier symbol count

    let mainCountAllLine =
        Core.countConsecutiveSymbols (fun x -> x = Config.MainGame.Wild) |> List.map

    let computeLineResult =
        Common.computeLineResult Game.phantomThiefPayLines mainCountAllLine calcLineWin

    let chooseGame (rng: unit -> float) =
        let chance = rng ()
        if chance <= 0.048 then MainGameA else MainGameB

    let countBonus (sequence: list<int * int * int>) =
        Common.countBonus (fun x -> x = Config.MainGame.Bonus) sequence

    let getReel (name: string) =
        if name = MainGameA then
            Config.MainGame.MainA, Config.MainGame.lensMainA
        else
            Config.MainGame.MainB, Config.MainGame.lensMainB

    let shoot reels idxMatrix =
        let ss = Core.snapshot reels idxMatrix
        let mul, lineResult = computeLineResult ss
        let bonus = countBonus (Core.lineup ss)
        idxMatrix, ss, mul, lineResult, bonus

    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Game.height rng
        shoot reels idxMatrix

    let collapse idxMatrix lineResult bonus reels lens =
        let idx =
            seq {
                yield! Common.allLineIdx Config.Line.lineMap lineResult
                yield! Common.allBonusIdx bonus
            }

        let newIdxMatrix = Collapse.collapse idxMatrix idx lens
        let ss = Core.snapshot reels newIdxMatrix
        let mul, lineResult = computeLineResult ss
        let bonus = countBonus (Core.lineup ss)
        newIdxMatrix, ss, mul, lineResult, bonus

    let freeSpin (bonusNum: int) = if bonusNum = 3 then 6 else 0
