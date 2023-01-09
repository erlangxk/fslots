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

    let freeSpin (bonusNum: int) =
        if bonusNum < Common.BONUS_MIN_COUNTS then 0 else 6

    let shoot reels idxMatrix =
        let ss = Core.snapshot reels idxMatrix
        let mul, lineResult = computeLineResult ss
        let bonus = countBonus (Core.lineup ss)
        idxMatrix, ss, mul, lineResult, bonus, (freeSpin bonus.Length)

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
        shoot reels newIdxMatrix

    let spinWithCollapse reels lens idx =
        let rec loopCollapse accCollapse accFreeSpin accMul oldIdxMatrix oldLineResult oldBonus =
            let idxMatrix, _, mul, lineResult, bonus, fs =
                collapse oldIdxMatrix oldLineResult oldBonus reels lens

            if (mul > 0 || fs > 0) then
                loopCollapse (accCollapse + 1) (accFreeSpin + fs) (accMul + mul) idxMatrix lineResult bonus
            else
                accCollapse, accFreeSpin, accMul

        let idxMatrix, _, spinLineMul, lineResult, bonus, spinFs = shoot reels idx

        let (collapse, collapseFs, collapseLineMul) =
            if (spinLineMul > 0 || spinFs > 0) then
                loopCollapse 1 0 0 idxMatrix lineResult bonus
            else
                (0, 0, 0)

        spinFs, spinLineMul, collapseFs, collapseLineMul, collapse
