module Slot.Web.Spin

open System.Text
open Falco
open FSharpPlus
open Slot.Web.GameRegistry

let ofJsonString (str: string) : HttpHandler =
    Response.withContentType "application/json; charset=utf-8"
    >> Response.ofString Encoding.UTF8 str

let notFound: HttpHandler = Response.withStatusCode 404 >> Response.ofEmpty

let jsonOrNotFound json =
    match json with
    | Some(json) -> ofJsonString json
    | None -> notFound

let gameMetaHandler: HttpHandler =
    fun ctx ->
        let r = Request.getRoute ctx

        let meta =
            monad {
                let! gameId = r.TryGetInt "Game"
                return! Map.tryFind gameId allGameMetas
            }

        jsonOrNotFound meta ctx

let gameSpinHandler: HttpHandler =
    fun ctx ->
        let r = Request.getRoute ctx

        let spin =
            monad {
                let! gameId = r.TryGetInt "Game"
                let! f = Map.tryFind gameId allGameSpins
                return f ()
            }

        jsonOrNotFound spin ctx
