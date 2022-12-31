namespace Slot.Games.PhantomThief

open Slot.Game.Prelude

module FeatureGame =
    let FeatureGameA = "FeatureGameA"
    let FeatureGameB = "FeatureGameB"

    let chooseGame (rng: unit->float) =
        let chance = rng ()
        if chance <= 0.0100 then FeatureGameA else FeatureGameB
        
    let randomFreeGame (rng: unit -> float) =
        let r = rng ()

        if r <= 0.3350 then 2
        elif r <= 0.6500 then 3
        else 4

    let freeSpin(bonusNum:int)(rng: unit -> float) =
            if bonusNum < 3 then 0 else randomFreeGame rng
    
    let calcLineWin symbol count =
        Config.FeatureGame.linePayTable |> Core.getNestedMultiplier symbol count

    let getReel (name: string) =
        if name = FeatureGameA then
            Config.FeatureGame.FeatureA, Config.FeatureGame.lensFeatureA
        else
            Config.FeatureGame.FeatureB, Config.FeatureGame.lensFeatureB

    let featureCountAllLine = Core.countConsecutiveSymbolsNoWild |> List.map

    let computeLinResult =
        Common.computeLineResult Game.phantomThiefPayLines featureCountAllLine calcLineWin

    let countBonus (sequence: list<int*int*int>) =
        Common.countBonus (fun x -> x = Config.FeatureGame.Bonus) sequence

    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Game.height rng
        let ss = Core.snapshot reels idxMatrix
        let (lineMul, lineResult) = computeLinResult ss
        let sequence = Core.lineup ss
        let bonus = countBonus sequence

        let (gemsMul, gemsResult) =
            Common.countGemsResult Config.FeatureGame.allGems Config.FeatureGame.gemsPayTable sequence

        idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus

    let collapse idxMatrix lineResult gemsResult bonus reels lens = 
        let idx = seq {
            yield! Common.allLineIdx Config.Line.lineMap lineResult
            yield! Common.allGemsIdx gemsResult
            yield! Common.allBonusIdx bonus
        }
        let newIdxMatrix = Collapse.collapse idxMatrix idx lens
        let ss = Core.snapshot reels newIdxMatrix
        let (lineMul, lineResult) = computeLinResult ss
        let sequence = Core.lineup ss
        let bonus = countBonus sequence

        let (gemsMul, gemsResult) =
            Common.countGemsResult Config.FeatureGame.allGems Config.FeatureGame.gemsPayTable sequence

        newIdxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus


