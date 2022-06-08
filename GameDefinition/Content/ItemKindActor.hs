-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( pattern S_COMPUTER_MOUSE
  , pattern HERO
  , pattern SCOUT_HERO
  , pattern BRAWLER_HERO
  , pattern SOLDIER_HERO
  , pattern CIVILIAN
  , pattern CAR
  , pattern COMPUTER
  , pattern SERVER
  , pattern ARMOR_RANGED
  , pattern STARTING_ARMOR
  , pattern STARTING_WEAPON
  , actorsGN
  , actorsGNSingleton
  , actors) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

import Content.ItemKindOrgan
import Content.ItemKindBlast

-- * Group name patterns

actorsGNSingleton :: [GroupName ItemKind]
actorsGNSingleton =
       [S_COMPUTER_MOUSE]

actorsGN :: [GroupName ItemKind]
actorsGN =
    [ HERO
    , SCOUT_HERO
    , BRAWLER_HERO
    , SOLDIER_HERO
    , CIVILIAN
    , CAR
    , COMPUTER
    , SERVER
    , ARMOR_RANGED
    , STARTING_ARMOR
    , STARTING_WEAPON ]

pattern HERO :: GroupName c
pattern HERO = GroupName "adventurer"
pattern SCOUT_HERO :: GroupName c
pattern SCOUT_HERO = GroupName "scout"
pattern BRAWLER_HERO :: GroupName c
pattern BRAWLER_HERO = GroupName "brawler"
pattern SOLDIER_HERO :: GroupName c
pattern SOLDIER_HERO = GroupName "soldier"
pattern CIVILIAN :: GroupName c
pattern CIVILIAN = GroupName "civilian"
pattern CAR :: GroupName c
pattern CAR = GroupName "car"
pattern COMPUTER :: GroupName c
pattern COMPUTER = GroupName "computer"
pattern SERVER :: GroupName c
pattern SERVER = GroupName "server"

pattern S_COMPUTER_MOUSE :: GroupName c
pattern S_COMPUTER_MOUSE = GroupName "computer mouse"

pattern ARMOR_RANGED :: GroupName c
pattern ARMOR_RANGED = GroupName "ranged armor"
pattern STARTING_ARMOR :: GroupName c
pattern STARTING_ARMOR = GroupName "starting armor"
pattern STARTING_WEAPON :: GroupName c
pattern STARTING_WEAPON = GroupName "starting weapon"

-- * Content

actors :: [ItemKind]
actors =
  [ warrior
  , warrior2
  , warrior3
  , warrior4
  , warrior5
  , scout
  , brawler
  , soldier
  , civilian
  , civilian2
  , civilian3
  , civilian4
  , civilian5
  , toyota
  , skoda
  , tesla
  , blackVolga
  , pc
  , laptop
  , macbook
  , server ]

-- * Hunams

humanOrgans :: [(GroupName ItemKind, CStore)]
humanOrgans = [ (S_FIST, COrgan), (S_FOOT, COrgan)
              , (S_EYE_6, COrgan), (S_EAR_6, COrgan)
              , (S_SAPIENT_BRAIN, COrgan) ]
warrior :: ItemKind
warrior = ItemKind
  { isymbol  = toContentSymbol '@'
  , iname    = "adventurer"  -- modified if initial actors in hero faction
  , ifreq    = [(HERO, 100), (MOBILE, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 80  -- partially from clothes and first aid
               , AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20
               , AddSkill SkNocto 2
               , AddSkill SkWait 1  -- can lurk
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 2  -- can even apply periodic items
               , AddSkill SkOdor 1
               , SetFlag Durable ]
  , ieffects = []
  , ikit     = humanOrgans
               ++ [(S_COMPUTER_MOUSE, CStash)]
  , idesc    = ""  -- "A hardened veteran of combat."
  }
warrior2 :: ItemKind
warrior2 = warrior
  { iname    = "warrior"
  , ikit     = humanOrgans
               ++ [(COMMON_ITEM, CStash)]
  -- , idesc    = ""
  }
warrior3 :: ItemKind
warrior3 = warrior
  { iname    = "blacksmith"
  -- , idesc    = ""
  }
warrior4 :: ItemKind
warrior4 = warrior
  { iname    = "forester"
  -- , idesc    = ""
  }
warrior5 :: ItemKind
warrior5 = warrior
  { iname    = "scientist"
  -- , idesc    = ""
  }

scout :: ItemKind
scout = warrior
  { ifreq    = [(SCOUT_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ARMOR_RANGED, CEqp) ]
  -- , idesc    = ""
  }
brawler :: ItemKind
brawler = warrior
  { ifreq    = [(BRAWLER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (STARTING_WEAPON, CEqp)
                  , (ANY_POTION, CStash) ]
  -- , idesc    = ""
  }
soldier :: ItemKind
soldier = brawler
  { ifreq    = [(SOLDIER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit brawler
               ++ [(EXPLOSIVE, CStash)]
  -- , idesc    = ""
  }

civilian :: ItemKind
civilian = warrior
  { iname    = "clerk"
  , ifreq    = [(CIVILIAN, 100), (MOBILE, 1)]
  , iflavour = zipPlain [BrBlack]
  -- , idesc    = ""
  }
civilian2 :: ItemKind
civilian2 = civilian
  { iname    = "hairdresser"
  -- , idesc    = ""
  }
civilian3 :: ItemKind
civilian3 = civilian
  { iname    = "lawyer"
  -- , idesc    = ""
  }
civilian4 :: ItemKind
civilian4 = civilian
  { iname    = "peddler"
  -- , idesc    = ""
  }
civilian5 :: ItemKind
civilian5 = civilian
  { iname    = "tax collector"
  -- , idesc    = ""
  }

-- * Cars

car :: ItemKind
car = ItemKind
  { isymbol  = toContentSymbol 't'
  , iname    = "car"
  , ifreq    = [(CAR, 100), (MOBILE, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "brum brum"
  , iweight  = 6000000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 28, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ OnSmash $ Explode S_VIOLENT_FRAGMENTATION ]
  , idesc    = "car description"
  , ikit     = [ (S_TIRE, COrgan)
               , (S_CAMERA, COrgan)
               , (S_CAR_COMPUTER, COrgan) ]
  }

skoda :: ItemKind
skoda = car
  { isymbol  = toContentSymbol 's'
  , iname    = "Skoda"
  , iweight  = 700000
  , iflavour = zipPlain [Green]
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 12, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , idesc    = "Czech vehicle of the neighbours"
  }

toyota :: ItemKind
toyota = car
  { isymbol  = toContentSymbol 't'
  , iname    = "Toyota"
  , iweight  = 800000
  , iflavour = zipPlain [Blue]
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , idesc    = "Classic vehicle of Japanese quality"
  }

tesla :: ItemKind
tesla = car
  { isymbol  = toContentSymbol 'T'
  , iname    = "Tesla"
  , iflavour = zipPlain [Red]
  , iweight  = 600000
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ OnSmash $ Explode S_LITHIUM_ION ] -- Batteries explode with colorful fire
  , idesc    = "New generation electric vehicle of questionable reputation"
  }

blackVolga :: ItemKind
blackVolga = car
  { isymbol  = toContentSymbol 'V'
  , iname    = "black Volga"
  , iflavour = zipPlain [BrBlack]
  , irarity  = [(1, 2), (10, 1)]
  , iweight  = 900000
  , iaspects = [ AddSkill SkMaxHP 50, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , idesc    = "Mysterious and legendary vehicle, who knows what it itentions are?"
  }

-- * Computers

computer :: ItemKind
computer = ItemKind
  { isymbol  = toContentSymbol '#'
  , iname    = "computer"
  , ifreq    = [(COMPUTER, 75), (SERVER, 25), (MOBILE, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "shhhhwuuuu"
  , iweight  = 5000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 10
               , AddSkill SkSpeed 8, AddSkill SkNocto 4
               , AddSkill SkAggression 5
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ ]
  , idesc    = "computer description"
  , ikit     = [ (S_CAMERA, COrgan), (S_CPU, COrgan), (S_EXHAUST_FAN, COrgan) ]
  }

pc :: ItemKind
pc = computer
  { isymbol  = toContentSymbol 'D'
  , iname    = "desktop computer"
  , iflavour = zipPlain [BrBlack]
  , iweight  = 6000
  , idesc    = "An old school desktop computer, perfect for writing Haskell programs."
  }

laptop :: ItemKind
laptop = computer
  { isymbol  = toContentSymbol 'L'
  , iname    = "laptop"
  , iflavour = zipPlain [BrRed]
  , iweight  = 2000
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 20
               , AddSkill SkSpeed 12, AddSkill SkNocto 3
               , AddSkill SkAggression 8
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ OnSmash $ Explode S_LITHIUM_ION ] -- Batteries explode with colorful fire
  , idesc    = "A swift laptop, great for playing Minecraft during boring lectures."
  }

macbook :: ItemKind
macbook = computer
  { isymbol  = toContentSymbol 'M'
  , iname    = "macbook"
  , iflavour = zipPlain [White]
  , iweight  = 1500
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 20
               , AddSkill SkSpeed 16, AddSkill SkNocto 3
               , AddSkill SkAggression 20
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ OnSmash $ Explode S_LITHIUM_ION ] -- Batteries explode with colorful fire
  , idesc    = "An ever thin and swift kind of laptop."
  }

server :: ItemKind
server = computer
  { isymbol  = toContentSymbol '#'
  , iname    = "server"
  , iflavour = zipPlain [Cyan]
  , iweight  = 2000
  , ifreq    = [(COMPUTER, 25), (SERVER, 75), (MOBILE, 1)]
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 40
               , AddSkill SkSpeed 4, AddSkill SkNocto 1
               , AddSkill SkAggression 5
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , idesc    = "A giant server rack. Unbearably loud."
  }
