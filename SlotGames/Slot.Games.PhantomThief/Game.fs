namespace Slot.Games.PhantomThief

open Slot.Game.Prelude
open FSharpPlus
open Slot.Games.PhantomThief.Common

module Line =

    let phantomThiefPayLines = Core.allPayLines Config.Line.allLines

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
        Common.computeLineResult Line.phantomThiefPayLines featureCountAllLine calcLineWin

    let countBonus (snapshot: int[][]) =
        Common.countBonus (fun x -> x = Config.FeatureGame.Bonus) snapshot


    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Config.height rng
        let ss = Core.snapshot reels idxMatrix
        let (lineMul, lineResult) = computeLinResult ss
        let bonus = countBonus ss

        let (gemsMul, gemsResult) =
            Common.countGemsResult Config.FeatureGame.allGems Config.FeatureGame.gemsPayTable ss

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
        let bonus = countBonus ss

        let (gemsMul, gemsResult) =
            Common.countGemsResult Config.FeatureGame.allGems Config.FeatureGame.gemsPayTable ss

        newIdxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus

module MainGame =

    let MainGameA = "MainGameA"
    let MainGameB = "MainGameB"

    let calcLineWin symbol count =
        Config.MainGame.linePayTable |> Core.getNestedMultiplier symbol count

    let mainCountAllLine =
        Core.countConsecutiveSymbols (fun x -> x = Config.MainGame.Wild) |> List.map

    let computeLinResult =
        Common.computeLineResult Line.phantomThiefPayLines mainCountAllLine calcLineWin

    let chooseGame (rng: unit->float) =
        let chance = rng ()
        if chance <= 0.048 then MainGameA else MainGameB

    let countBonus (snapshot: int[][]) =
        Common.countBonus (fun x -> x = Config.MainGame.Bonus) snapshot

    let getReel (name: string) =
        if name = MainGameA then
            Config.MainGame.MainA, Config.MainGame.lensMainA
        else
            Config.MainGame.MainB, Config.MainGame.lensMainB

    let shoot reels idxMatrix =
        let ss = Core.snapshot reels idxMatrix
        let (mul, lineResult) = computeLinResult ss
        let bonus = countBonus ss
        idxMatrix, ss, mul, lineResult, bonus
    
    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Config.height rng
        shoot reels idxMatrix

    let collapse idxMatrix lineResult bonus reels lens =
        let idx = seq {
           yield! Common.allLineIdx Config.Line.lineMap lineResult
           yield! Common.allBonusIdx bonus
        }
        let newIdxMatrix = Collapse.collapse idxMatrix idx lens
        let ss = Core.snapshot reels newIdxMatrix
        let (mul, lineResult) = computeLinResult ss
        let bonus = countBonus ss
        newIdxMatrix, ss, mul, lineResult, bonus

    let freeSpin(bonusNum:int) = if bonusNum = 3 then 6 else 0
module State =
    open Common

    let firstGameAction (rng: unit -> float) =
        true, MainGame.chooseGame rng, Action.Spin, None

    let nextGameAction (gameState: GameState) (rng: unit -> float) =
        if (gameState.lineMul + gameState.gemsMul = 0 && gameState.bonus.Length < 3) then
            if (gameState.freeSpin <> 0) then
                false, FeatureGame.chooseGame rng, Action.Spin, None
            else
                true, MainGame.chooseGame rng, Action.Spin, None

        else
            gameState.mainGame, gameState.name, Action.Collapse, Some(gameState)

    let loadState (state: option<GameState>) (rng: unit -> float) =
        match state with
        | None -> firstGameAction (rng)
        | Some(s) -> nextGameAction (s) (rng)


    let resume
        (gameState:option<GameState>)
        (rng1: int -> int)
        (rng2: unit -> float)
        :GameState =
        
        let mainGame,gameName, action, state =
            match gameState with
            |Some(gs) -> nextGameAction gs rng2
            |None -> firstGameAction rng2
        
        if (mainGame) then
            let reels, lens = MainGame.getReel gameName
            match action with
             | Action.Spin->
                let spinResult = MainGame.spin reels lens rng1
                let idxMatrix, ss, mul, lineResult, bonus = spinResult
                let freeSpin = MainGame.freeSpin bonus.Length
                { mainGame = true
                  freeSpin = freeSpin
                  name = gameName
                  idxMatrix = idxMatrix
                  snapshot = ss
                  lineMul = mul
                  lineResult = lineResult
                  gemsMul = 0
                  gemsResult = []
                  bonus = bonus }
             | Action.Collapse ->
                let s = Option.get state
                let cascadeResult = MainGame.collapse s.idxMatrix s.lineResult s.bonus reels lens
                let idxMatrix, ss, mul, lineResult, bonus = cascadeResult
                let freeSpin = MainGame.freeSpin bonus.Length
                { s with
                    snapshot = ss
                    idxMatrix = idxMatrix
                    lineMul = mul
                    lineResult = lineResult
                    bonus = bonus
                    freeSpin = s.freeSpin + freeSpin }

        else
            let reels, lens = FeatureGame.getReel gameName
            match action with 
              |Action.Spin ->
                let spinResult = FeatureGame.spin reels lens rng1
                let idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus = spinResult
                let freeSpin = FeatureGame.freeSpin bonus.Length  rng2   
                { mainGame = true
                  freeSpin = freeSpin
                  name = gameName
                  idxMatrix = idxMatrix
                  snapshot = ss
                  lineMul = lineMul
                  lineResult = lineResult
                  gemsMul = gemsMul
                  gemsResult = gemsResult
                  bonus = bonus}
              |Action.Collapse ->
                let s = Option.get state
                let cascadeResult = FeatureGame.collapse s.idxMatrix s.lineResult s.gemsResult s.bonus reels lens
                let idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus = cascadeResult
                let freeSpin = FeatureGame.freeSpin bonus.Length rng2
                { s with
                    snapshot = ss
                    idxMatrix = idxMatrix
                    lineMul = lineMul
                    lineResult = lineResult
                    freeSpin = s.freeSpin + freeSpin
                    gemsMul = gemsMul
                    gemsResult = gemsResult
                    bonus = bonus }