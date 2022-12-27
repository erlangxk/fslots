module Slot.Web.GameRegistry

open Slot.Games
open Slot.Games.QueenBee.ResultJson
open BlitzkriegSoftware.SecureRandomLibrary
open Thoth.Json.Net

let rng =
    let sr = SecureRandom()
    fun max -> sr.Next(0, max)

let allGameSpins =
    Map [ 1, (fun () -> encodeResult (QueenBee.Pack.randomSpinLevel1 rng)) ]

let allGameMetas = Map [ 1, Encode.Auto.toString (0, QueenBee.Pack.meta) ]
