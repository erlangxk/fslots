namespace Slot.Games.PhantomThief

open Slot.Game.Prelude

module Game =
    let width, height = 5, 3

    let phantomThiefPayLines (snapshot: 'a[][]) =
        Core.allPayLines Config.Line.allLines snapshot
