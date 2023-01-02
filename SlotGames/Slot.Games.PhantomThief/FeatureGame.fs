namespace Slot.Games.PhantomThief

open Slot.Game.Prelude

module FeatureGame =
    let FeatureGameA = "FeatureGameA"
    let FeatureGameB = "FeatureGameB"

    let chooseGame (rng: unit -> float) =
        let chance = rng ()
        if chance <= 0.0100 then FeatureGameA else FeatureGameB

    let randomFreeGame (rng: unit -> float) =
        let r = rng ()

        if r <= 0.3350 then 2
        elif r <= 0.6500 then 3
        else 4

    let freeSpin (bonusNum: int) (rng: unit -> float) =
        if bonusNum < 3 then 0 else randomFreeGame rng

    let calcLineWin symbol count =
        Config.FeatureGame.linePayTable |> Core.getNestedMultiplier symbol count

    let getReel (name: string) =
        if name = FeatureGameA then
            Config.FeatureGame.FeatureA, Config.FeatureGame.lensFeatureA
        else
            Config.FeatureGame.FeatureB, Config.FeatureGame.lensFeatureB

    let featureCountAllLine = Core.countConsecutiveSymbolsNoWild |> List.map

    let computeLineResult =
        Common.computeLineResult Game.phantomThiefPayLines featureCountAllLine calcLineWin

    let computeGemsResult =
        Common.countGemsResult Config.FeatureGame.allGems Config.FeatureGame.gemsPayTable

    let countBonus (sequence: list<int * int * int>) =
        Common.countBonus (fun x -> x = Config.FeatureGame.Bonus) sequence

    let shoot reels idxMatrix =
        let ss = Core.snapshot reels idxMatrix
        let lineMul, lineResult = computeLineResult ss
        let sequence = Core.lineup ss
        let bonus = countBonus sequence
        let gemsMul, gemsResult = computeGemsResult sequence

        idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus

    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Game.height rng
        shoot reels idxMatrix

    let collapse idxMatrix lineResult gemsResult bonus reels lens =
        let idx =
            seq {
                yield! Common.allLineIdx Config.Line.lineMap lineResult
                yield! Common.allGemsIdx gemsResult
                yield! Common.allBonusIdx bonus
            }

        let newIdxMatrix = Collapse.collapse idxMatrix idx lens
        shoot reels newIdxMatrix

    let spinWithCollapse reels lens rng idx=
        let rec loopCollapse
            accCollapse
            accFreeSpin
            accLineMul
            accGemsMul
            oldIdxMatrix
            oldLineResult
            oldGemsResult
            oldBonus
            =
            let idxMatrix, _, lineMul, lineResult, gemsMul, gemsResult, bonus =
                collapse oldIdxMatrix oldLineResult oldGemsResult oldBonus reels lens

            let fs = freeSpin bonus.Length rng

            if ((lineMul + gemsMul) > 0 || fs > 0) then
                loopCollapse
                    (accCollapse + 1)
                    (accFreeSpin + fs)
                    (accLineMul + lineMul)
                    (accGemsMul + gemsMul)
                    idxMatrix
                    lineResult
                    gemsResult
                    bonus
            else
                accCollapse, accFreeSpin, accLineMul, accGemsMul

        let idxMatrix, _, spinLineMul, lineResult, spinGemsMul, gemsResult, bonus = shoot reels idx
        let spinFs = freeSpin bonus.Length rng

        let (collapseCount, collapseFs, collapseLineMul, collapseGemsMul) =
            if (spinLineMul + spinGemsMul > 0 || spinFs > 0) then
                loopCollapse 1 0 0 0 idxMatrix lineResult gemsResult bonus
            else
                (0, 0, 0, 0)

        spinFs, spinLineMul, spinGemsMul, collapseFs, collapseLineMul, collapseGemsMul, collapseCount
