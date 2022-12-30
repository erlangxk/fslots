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

    let collapse idxMatrix lineResult gemsResult reels lens = 
        let idx = seq {
            yield! Common.winIdx Config.Line.lineMap lineResult
            yield! Common.allGemsPos gemsResult
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

    let spin reels lens rng =
        let idxMatrix = Core.randomReelIdx lens Config.height rng
        let ss = Core.snapshot reels idxMatrix
        let (mul, lineResult) = computeLinResult ss
        let bonus = countBonus ss
        idxMatrix, ss, mul, lineResult, bonus

    let collapse idxMatrix lineResult reels lens =
        let lineCs = Common.winIdx Config.Line.lineMap lineResult
        let newIdxMatrix = Collapse.collapse idxMatrix lineCs lens
        let ss = Core.snapshot reels newIdxMatrix
        let (mul, lineResult) = computeLinResult ss
        let bonus = countBonus ss
        newIdxMatrix, ss, mul, lineResult, bonus

module State =
    open Common

    let firstGameAction (rng: unit -> float) =
        true, MainGame.chooseGame rng, Action.Spin, None

    let nextGameAction (gameState: GameState) (rng: unit -> float) =
        if (gameState.lineMul + gameState.gemsMul = 0) then
            if (gameState.freeSpin <> 0) then
                false, FeatureGame.chooseGame rng, Action.Spin, None
            else
                true, MainGame.chooseGame rng, Action.Spin, None

        else
            gameState.mainGame, gameState.name, Action.Cascade, Some(gameState)

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
            if action = Action.Spin then
                let spinResult = MainGame.spin reels lens rng1
                let idxMatrix, ss, mul, lineResult, bonus = spinResult
                let freeSpin = if bonus = 3 then 6 else 0
                { mainGame = true
                  freeSpin = freeSpin
                  name = gameName
                  idxMatrix = idxMatrix
                  snapshot = ss
                  lineMul = mul
                  lineResult = lineResult
                  gemsMul = 0
                  gemsResult = [] }
            else
                let s = Option.get state
                let cascadeResult = MainGame.collapse s.idxMatrix s.lineResult reels lens
                let idxMatrix, ss, mul, lineResult, bonus = cascadeResult
                let freeSpin = if bonus = 3 then 6 else 0

                { s with
                    snapshot = ss
                    idxMatrix = idxMatrix
                    lineMul = mul
                    lineResult = lineResult
                    freeSpin = s.freeSpin + freeSpin }

        else
            let reels, lens = FeatureGame.getReel gameName

            if action = Action.Spin then
                let spinResult = FeatureGame.spin reels lens rng1
                let idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus = spinResult
                let freeSpin =
                    if bonus = 3 then
                        Common.randomFreeGame rng2
                    else
                        0
                { mainGame = true
                  freeSpin = freeSpin
                  name = gameName
                  idxMatrix = idxMatrix
                  snapshot = ss
                  lineMul = lineMul
                  lineResult = lineResult
                  gemsMul = gemsMul
                  gemsResult = gemsResult }
            else
                let s = Option.get state
                let cascadeResult = FeatureGame.collapse s.idxMatrix s.lineResult s.gemsResult reels lens
                let idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus = cascadeResult
                let freeSpin =
                    if bonus = 3 then
                        Common.randomFreeGame rng2
                    else
                        0
                { s with
                    snapshot = ss
                    idxMatrix = idxMatrix
                    lineMul = lineMul
                    lineResult = lineResult
                    freeSpin = s.freeSpin + freeSpin
                    gemsMul = gemsMul
                    gemsResult = gemsResult }