module Slot.Web.Spin

open System.Text
open Falco
open Slot.Games.QueenBee.Game
open Slot.Games.QueenBee.ResultJson


let ofJsonString (str : string) : HttpHandler =
    Response.withContentType "application/json; charset=utf-8"
    >> Response.ofString Encoding.UTF8 str
    
let playerSpinHandler rng: HttpHandler =
    fun ctx ->
        let r = encodeResult(Core.randomSpinLevel1 rng)
        ofJsonString r ctx