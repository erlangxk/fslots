namespace Slot.Games.PhantomThief

open Common

module GameState =
    type Action =
        | Spin
        | Collapse


    type GameState =
        { mainGame: bool
          freeSpin: int
          name: string
          idxMatrix: list<list<int>>
          snapshot: int[][]
          lineMul: int
          lineResult: LineWinResult<int>
          gemsMul: int
          gemsResult: GemWinResult<int>
          bonus: list<int * int> }

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


    let resume (gameState: option<GameState>) (rng1: int -> int) (rng2: unit -> float) : GameState =

        let mainGame, gameName, action, state =
            match gameState with
            | Some(gs) -> nextGameAction gs rng2
            | None -> firstGameAction rng2

        if (mainGame) then
            let reels, lens = MainGame.getReel gameName

            match action with
            | Action.Spin ->
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
            | Action.Spin ->
                let spinResult = FeatureGame.spin reels lens rng1
                let idxMatrix, ss, lineMul, lineResult, gemsMul, gemsResult, bonus = spinResult
                let freeSpin = FeatureGame.freeSpin bonus.Length rng2

                { mainGame = true
                  freeSpin = freeSpin
                  name = gameName
                  idxMatrix = idxMatrix
                  snapshot = ss
                  lineMul = lineMul
                  lineResult = lineResult
                  gemsMul = gemsMul
                  gemsResult = gemsResult
                  bonus = bonus }
            | Action.Collapse ->
                let s = Option.get state

                let cascadeResult =
                    FeatureGame.collapse s.idxMatrix s.lineResult s.gemsResult s.bonus reels lens

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
