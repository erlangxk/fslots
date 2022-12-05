module Slot.Games.QueenBee.Symbols

open FSharpPlus
module Symbol =
    let Ten = 0
    let Jack = 1
    let Queen = 2
    let King = 3
    let Ace = 4
    let Daisy = 5
    let Flower =6
    let BeeHives = 7
    let SkinnyBee = 8
    let FatBee = 9
    let Scatter = 10
    let Wild = 11


let fatBee    = Map [(5,5000);(4,500);(3,100);(2,10);(1,2)]
let skinnyBee = Map [(5,2500);(4,250);(3,50);(2,5)]
let beeHives =  Map [(5,1000);(4,100);(3,20);(2,3)]
let flower   =  Map [(5,1000);(4,100);(3,20);(2,3)]
let daisy = Map [(5,500);(4,30);(3,10)]
let ace   = Map [(5,300);(4,25);(3,5)]
let king  = Map [(5,200);(4,20);(3,5)]
let queen = Map [(5,200);(4,20);(3,5)]
let jack  = Map [(5,100);(4,15);(3,5)]
let ten   = Map [(5,100);(4,15);(3,5)]

let scatter = Map [(5,100);(4,10);(3,5);(2,1)]

let plainPayTable:Map<int, Map<int,int>> = Map[
    Symbol.FatBee,fatBee;
    Symbol.SkinnyBee, skinnyBee;
    Symbol.BeeHives, beeHives;
    Symbol.Flower, flower;
    Symbol.Daisy, daisy;
    Symbol.Ace, ace;
    Symbol.King, king;
    Symbol.Queen, queen;
    Symbol.Jack, jack;
    Symbol.Ten, ten
]

let ScatterId = Symbol.Scatter
let WildId = Symbol.Wild

let inline simpleLookup table count= Map.tryFind count table

let nestedLookup table symbol count=
    monad {
      let! t = Map.tryFind symbol table
      return! simpleLookup t count
    }
    
let queenBeeScatterWin count = simpleLookup  count scatter
     
let queenBeePlainWin  = nestedLookup plainPayTable 