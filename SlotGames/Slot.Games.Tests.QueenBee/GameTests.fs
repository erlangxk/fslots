module Slot.Games.Tests.QueenBee.GameTests


open NUnit.Framework
open Slot.Games.QueenBee
open System.Linq

let random = System.Random()

[<Test>]
let testSpin() =
    let rng i =  random.Next(0,i)
    let result = Game.Core.spinLevel1 rng
    
    printf $"wild = {Game.PayTable.Wild}\n"
    printf $"scatter = {Game.PayTable.Scatter}\n"
    printf $"{result}"
   

    