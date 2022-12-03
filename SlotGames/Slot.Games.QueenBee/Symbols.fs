module Slot.Games.QueenBee.Symbols

type Symbol =
    | Ten = 0
    | Jack = 1
    | Queen = 2
    | King = 3
    | Ace = 4
    | Daisy = 5
    | Flower =6
    | BeeHives = 7
    | SkinnyBee = 8
    | FatBee = 9
    | Scatter = 10
    | Wild = 11


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

let payTable = Map[
    (Symbol.FatBee,fatBee);
    (Symbol.SkinnyBee, skinnyBee);
    (Symbol.BeeHives, beeHives);
    (Symbol.Flower, flower);
    (Symbol.Daisy, daisy);
    (Symbol.Ace, ace);
    (Symbol.King, king);
    (Symbol.Queen, queen);
    (Symbol.Jack, jack);
    (Symbol.Ten, ten)
]

let ScatterId = int Symbol.Scatter
let WildId = int Symbol.Wild