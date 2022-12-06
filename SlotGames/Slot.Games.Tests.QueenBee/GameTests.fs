module Slot.Games.Tests.QueenBee.GameTests


open NUnit.Framework
open Slot.Games.QueenBee
open System.Linq

let random = System.Random()

[<Test>]
let testSpin() =
    let rng i =  random.Next(0,i)
    
    let times = 1000000
    let mutable total:double = 0.0
    for i in 1..times do
        let r = Game.Core.spinLevel1 rng
        let m = r.plain.multiplier + r.scatter.multiplier
        total <- total + (double)m
    
    printf $"RTP = {total/(double)times}"
     
   
   

    