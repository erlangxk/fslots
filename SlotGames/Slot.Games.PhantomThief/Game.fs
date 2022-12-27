namespace Slot.Games.PhantomThief


module Line =
    let lines =
        [ [ 1; 1; 1; 1; 1 ]
          [ 0; 0; 0; 0; 0 ]
          [ 2; 2; 2; 2; 2 ]
          [ 0; 0; 1; 1; 1 ]
          [ 2; 2; 1; 1; 1 ]
          [ 0; 0; 2; 2; 2 ]
          [ 2; 2; 0; 0; 0 ]
          [ 1; 1; 0; 1; 2 ]
          [ 1; 1; 2; 1; 0 ]
          [ 0; 1; 0; 0; 0 ]
          [ 2; 1; 2; 2; 2 ]
          [ 1; 0; 0; 0; 1 ]
          [ 1; 2; 2; 2; 1 ]
          [ 1; 0; 1; 0; 0 ]
          [ 1; 2; 1; 2; 2 ]
          [ 0; 1; 1; 2; 2 ]
          [ 2; 1; 1; 0; 0 ]
          [ 1; 0; 2; 1; 1 ]
          [ 1; 2; 0; 1; 1 ]
          [ 0; 1; 2; 2; 2 ]
          [ 2; 1; 0; 0; 0 ]
          [ 0; 2; 0; 1; 2 ]
          [ 2; 0; 2; 1; 0 ]
          [ 0; 2; 1; 0; 1 ]
          [ 2; 0; 1; 2; 1 ]
          [ 0; 2; 2; 0; 0 ]
          [ 2; 0; 0; 2; 2 ]
          [ 1; 1; 1; 0; 0 ]
          [ 1; 1; 1; 2; 2 ]
          [ 0; 2; 2; 2; 2 ] ]
        
    let totalLines = lines.Length
        

module FreeGame =
    let Crossbow,Gauntlet,Hammer,Helmet = 0,1,2,3
    let GreenGem,BlueGem,OrangeGem,Police,Bonus = 4,5,6,7,8
    
    let LinePayTable = Map[
        Crossbow, Map[(5,65); (4,20); (3,5)]
        Gauntlet, Map[(5,80); (4,25); (3,10)]
        Hammer,   Map[(5,80); (4,25); (3,10)]
        Helmet,   Map[(5,100);(4,35); (3,15)]
        GreenGem, Map[(5,5);  (4,3);  (3,1)]
        BlueGem,  Map[(5,8);  (4,5);  (3,2)]
        OrangeGem,Map[(5,15); (4,8);  (3,3)]
        Police,   Map[(5,800);(4,300);(3,50)]
    ]
    
    let FreeA =[        
        [|GreenGem;Crossbow;Gauntlet;Helmet;Helmet;Police;Bonus;Hammer;OrangeGem;Crossbow;GreenGem;Gauntlet;BlueGem;Hammer;Helmet;OrangeGem;Crossbow;Helmet|]
        [|Hammer;OrangeGem;Crossbow;BlueGem;Gauntlet;Helmet;Bonus;Helmet;Crossbow;GreenGem;Helmet;Hammer;Crossbow;BlueGem;Gauntlet;Crossbow;Police;Gauntlet|]
        [|BlueGem;Crossbow;Helmet;Hammer;Helmet;Crossbow;Bonus;OrangeGem;Helmet;BlueGem;Crossbow;Crossbow;Gauntlet;Police;Crossbow;Hammer;GreenGem;Gauntlet|]
        [|Gauntlet;Police;Hammer;Crossbow;OrangeGem;Gauntlet;BlueGem;Hammer;Crossbow;Crossbow;Police;Helmet;GreenGem;Gauntlet;OrangeGem;Crossbow;Helmet|]
        [|Police;Hammer;Gauntlet;BlueGem;Hammer;GreenGem;Gauntlet;OrangeGem;Helmet;GreenGem;Gauntlet;OrangeGem;Hammer;Police;Crossbow;BlueGem;Hammer|]
    ]
    
    let FreeB = [
        [|GreenGem;Gauntlet;Crossbow;GreenGem;Bonus;Helmet;Crossbow;Gauntlet;Police;Hammer;BlueGem;Crossbow;OrangeGem;Bonus;Crossbow;BlueGem;Hammer;OrangeGem;Helmet;Crossbow;BlueGem;Hammer;Bonus;Gauntlet;BlueGem;Hammer;OrangeGem;Crossbow;Gauntlet;Helmet;Hammer;Gauntlet;OrangeGem;Helmet|]
        [|Hammer;OrangeGem;BlueGem;GreenGem;Bonus;Gauntlet;Crossbow;GreenGem;BlueGem;Helmet;Crossbow;GreenGem;Hammer;Bonus;Helmet;Crossbow;BlueGem;Gauntlet;Helmet;OrangeGem;Police;Bonus;Gauntlet;GreenGem;Crossbow;Gauntlet;OrangeGem;Crossbow;Bonus;GreenGem;Helmet;Hammer;Crossbow;Police;Gauntlet;Helmet|]
        [|BlueGem;Crossbow;GreenGem;Helmet;Bonus;Hammer;Crossbow;BlueGem;OrangeGem;Helmet;BlueGem;Crossbow;Hammer;Gauntlet;Bonus;Police;Crossbow;GreenGem;Gauntlet;OrangeGem;Crossbow;Bonus;GreenGem;Helmet;Gauntlet;GreenGem;Crossbow;Helmet;Police;Gauntlet;OrangeGem;Hammer;Gauntlet;Helmet|]
        [|Gauntlet;Police;Hammer;GreenGem;Crossbow;OrangeGem;Gauntlet;BlueGem;Hammer;Police;GreenGem;Helmet;Gauntlet;OrangeGem;Hammer;Gauntlet;BlueGem;Helmet;Gauntlet;GreenGem;Hammer;Helmet;BlueGem;Police;Gauntlet;Crossbow;GreenGem;Hammer;Crossbow;BlueGem;Helmet;OrangeGem;Crossbow|]
        [|Police;Hammer;Gauntlet;BlueGem;Helmet;Hammer;Gauntlet;GreenGem;OrangeGem;Helmet;Gauntlet;GreenGem;Hammer;OrangeGem;Crossbow;GreenGem;Gauntlet;BlueGem;Hammer;Helmet;OrangeGem;Crossbow;BlueGem;Hammer;GreenGem;Helmet;OrangeGem;BlueGem;Crossbow;GreenGem;Helmet|]
    ]
        
module MainGame =
    let Jack,Queen,King,Ace = 0,1,2,3
    let Sceptre,Goblet,Crown,Thief = 4,5,6,7
    let Bonus,Wild = 8,9
    
    let LinePayTable = Map[
        Jack,   Map[(5,65);(4,20);(3,5)]
        Queen,  Map[(5,80);(4,25);(3,10)]
        King,   Map[(5,80);(4,25);(3,10)]
        Ace,    Map[(5,100);(4,35);(3,15)]
        Sceptre,Map[(5,125);(4,40);(3,20)]
        Goblet, Map[(5,150);(4,50);(3,25)]
        Crown,  Map[(5,200);(4,60);(3,30)]
        Thief,  Map[(5,800);(4,300);(3,50)]
    ]
    
    let GameA =[
        [|Sceptre;Jack;Queen;Bonus;Ace;Thief;King;Jack;Crown;Queen;Sceptre;Goblet;King;Crown;Ace;Jack;Ace|]       
        [|King;Crown;Goblet;Jack;Queen;Bonus;Ace;Jack;Sceptre;Ace;King;Goblet;Queen;Jack;Thief;Queen|]
        [|Goblet;Jack;Ace;King;Bonus;Jack;Crown;Ace;Goblet;Jack;King;Queen;Jack;Thief;Sceptre;Queen |]
        [|Queen;Thief;King;Jack;Crown;Goblet;Queen;King;Jack;Thief;Sceptre;Ace;Queen;Crown;Jack;Ace |]
        [|Thief;King;Queen;Goblet;King;Queen;Sceptre;Crown;Ace;Queen;Sceptre;King;Crown;Jack;Goblet;Thief;King|]
    ]
    
    let GameB = [
        [|Sceptre;Jack;Queen;Bonus;Ace;Thief;King;Jack;Crown;Queen;Sceptre;Goblet;King;Bonus;Crown;Ace;Jack;Thief;King;Queen;Goblet;King;Thief;Ace;Crown;Jack;Ace;Thief;Queen;Goblet;King;Queen;Ace|]
        [|King;Crown;Goblet;Jack;Queen;Bonus;Ace;Jack;Sceptre;Ace;King;Wild;Goblet;Queen;Jack;Ace;Queen;Crown;Thief;Bonus;Sceptre;Goblet;Queen;Crown;Jack;Sceptre;King;Ace;Goblet;King;Bonus;Jack;Thief;Queen;Jack;Sceptre;Ace;King;Jack;Thief;Queen;Jack|]
        [|Goblet;Jack;Ace;King;Bonus;Jack;Crown;Ace;Goblet;Jack;King;Queen;Wild;Jack;Thief;Sceptre;Queen;Jack;Thief;Ace;Queen;Sceptre;Jack;Thief;Ace;Queen;Bonus;Thief;King;Jack;Crown;King;Queen;Sceptre;Ace;Queen|]
        [|Queen;Thief;King;Jack;Crown;Goblet;Queen;King;Wild;Jack;Thief;Sceptre;Ace;Queen;Crown;Jack;Ace;Thief;Queen;King;Goblet;Queen;Sceptre;King;Ace;Sceptre;Queen;Goblet;Ace;Jack;Thief;Sceptre;King;Jack;Goblet;Crown;Jack|]
        [|Thief;King;Queen;Goblet;King;Queen;Sceptre;Crown;Ace;Queen;Sceptre;King;Crown;Jack;Goblet;Thief;King;Queen;Crown;Sceptre;Ace;King;Goblet;Jack;Ace;Crown;Jack;Goblet;King;Crown;Jack;Sceptre;Thief;Goblet;Ace;Jack|]
    ]

module Game =
    let width,height = 5,3
    
    