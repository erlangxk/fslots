module Slot.Web.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Microsoft.AspNetCore.Builder
open Slot.Web.Spin
open BlitzkriegSoftware.SecureRandomLibrary


// ------------
// Exception Handler
// ------------
let exceptionHandler : HttpHandler =
    Response.withStatusCode 500 
    >> Response.ofPlainText "Server error"
    

let rng  =
    let sr = SecureRandom()
    fun max -> sr.Next(0,max)

[<EntryPoint>]
let main args =   
    webHost args {
        use_if    FalcoExtensions.IsDevelopment DeveloperExceptionPageExtensions.UseDeveloperExceptionPage
        use_ifnot FalcoExtensions.IsDevelopment (FalcoExtensions.UseFalcoExceptionHandler exceptionHandler)
        
        endpoints [            
            get "/" (Response.ofPlainText $"Hello world{rng 32}")
            get "/spin" (playerSpinHandler rng)
        ]
    }
    0