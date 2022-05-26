-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( pattern S_WOODEN_TORCH
  , pattern S_SANDSTONE_ROCK
  , pattern HERO
  , pattern SCOUT_HERO
  , pattern RANGER_HERO
  , pattern ESCAPIST_HERO
  , pattern AMBUSHER_HERO
  , pattern BRAWLER_HERO
  , pattern SOLDIER_HERO
  , pattern CIVILIAN
  , pattern MONSTER
  , pattern MOBILE_MONSTER
  , pattern SCOUT_MONSTER
  , pattern CAR
  , pattern ADD_SIGHT, pattern ARMOR_RANGED, pattern ADD_NOCTO_1
  , pattern WEAK_ARROW
  , pattern LIGHT_ATTENUATOR
  , pattern RING_OF_OPPORTUNITY_SNIPER
  , pattern ANY_ARROW
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
       [S_WOODEN_TORCH, S_SANDSTONE_ROCK]

actorsGN :: [GroupName ItemKind]
actorsGN =
    [ HERO
    , SCOUT_HERO
    , RANGER_HERO
    , ESCAPIST_HERO
    , AMBUSHER_HERO
    , BRAWLER_HERO
    , SOLDIER_HERO
    , CIVILIAN
    , MONSTER
    , MOBILE_MONSTER
    , SCOUT_MONSTER
    , CAR
    , MOBILE_CAR
    , ADD_SIGHT
    , ARMOR_RANGED
    , ADD_NOCTO_1
    , WEAK_ARROW
    , LIGHT_ATTENUATOR
    , RING_OF_OPPORTUNITY_SNIPER
    , ANY_ARROW
    , STARTING_ARMOR
    , STARTING_WEAPON ]

pattern HERO :: GroupName c
pattern HERO = GroupName "adventurer"
pattern SCOUT_HERO :: GroupName c
pattern SCOUT_HERO = GroupName "scout"
pattern RANGER_HERO :: GroupName c
pattern RANGER_HERO = GroupName "ranger"
pattern ESCAPIST_HERO :: GroupName c
pattern ESCAPIST_HERO = GroupName "escapist"
pattern AMBUSHER_HERO :: GroupName c
pattern AMBUSHER_HERO = GroupName "ambusher"
pattern BRAWLER_HERO :: GroupName c
pattern BRAWLER_HERO = GroupName "brawler"
pattern SOLDIER_HERO :: GroupName c
pattern SOLDIER_HERO = GroupName "soldier"
pattern CIVILIAN :: GroupName c
pattern CIVILIAN = GroupName "civilian"
pattern MONSTER :: GroupName c
pattern MONSTER = GroupName "monstrosity"
pattern MOBILE_MONSTER :: GroupName c
pattern MOBILE_MONSTER = GroupName "mobile monstrosity"
pattern SCOUT_MONSTER :: GroupName c
pattern SCOUT_MONSTER = GroupName "scout monstrosity"
pattern CAR :: GroupName c
pattern CAR = GroupName "car"
pattern MOBILE_CAR :: GroupName c
pattern MOBILE_CAR = GroupName "mobile car"

pattern S_WOODEN_TORCH :: GroupName c
pattern S_WOODEN_TORCH = GroupName "wooden torch"
pattern S_SANDSTONE_ROCK :: GroupName c
pattern S_SANDSTONE_ROCK = GroupName "sandstone rock"

pattern ADD_SIGHT :: GroupName c
pattern ADD_SIGHT = GroupName "sight improvement"
pattern ARMOR_RANGED :: GroupName c
pattern ARMOR_RANGED = GroupName "ranged armor"
pattern ADD_NOCTO_1 :: GroupName c
pattern ADD_NOCTO_1 = GroupName "noctovision improvement"
pattern WEAK_ARROW :: GroupName c
pattern WEAK_ARROW = GroupName "weak arrow"
pattern LIGHT_ATTENUATOR :: GroupName c
pattern LIGHT_ATTENUATOR = GroupName "light attenuator"
pattern RING_OF_OPPORTUNITY_SNIPER :: GroupName c
pattern RING_OF_OPPORTUNITY_SNIPER = GroupName "ring of sniper"
pattern ANY_ARROW :: GroupName c
pattern ANY_ARROW = GroupName "arrow"
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
  , ranger
  , escapist
  , ambusher
  , brawler
  , soldier
  , civilian
  , civilian2
  , civilian3
  , civilian4
  , civilian5
  , eye
  , fastEye
  , nose
  , elbow
  , torsor
  , toyota
  , skoda
  , tesla
  , blackVolga ]

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

-- TODO: bring back S_EAR_3 when character progression permits hearing boosts.
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
               ++ [(S_SANDSTONE_ROCK, CStash)]
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
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (ADD_NOCTO_1, CStash) ]
  -- , idesc    = ""
  }
ranger :: ItemKind
ranger = warrior
  { ifreq    = [(RANGER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  -- , idesc    = ""
  }
escapist :: ItemKind
escapist = warrior
  { ifreq    = [(ESCAPIST_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ADD_SIGHT, CEqp)
                  , (STARTING_ARMOR, CEqp)
                  , (WEAK_ARROW, CStash)  -- mostly for probing
                  , (LIGHT_ATTENUATOR, CStash)
                  , (S_WOODEN_TORCH, CStash) ]
  -- , idesc    = ""
  }
ambusher :: ItemKind
ambusher = warrior
  { ifreq    = [(AMBUSHER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- dark and numerous, so more kit without exploring
               ++ [ (RING_OF_OPPORTUNITY_SNIPER, CEqp)
                  , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
                  , (WEAK_ARROW, CStash)
                  , (EXPLOSIVE, CStash)
                  , (LIGHT_ATTENUATOR, CEqp)
                  , (S_WOODEN_TORCH, CStash) ]
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

-- * Monsters

-- They have bright colours, because they are not natural.

eye :: ItemKind
eye = ItemKind
  { isymbol  = toContentSymbol 'e'
  , iname    = "reducible eye"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 10) ]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(3, 0), (4, 10), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 16, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [ (S_LASH, COrgan), (S_PUPIL, COrgan)  -- at least one non-timed
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no hearing, it's all eyes
  }
fastEye :: ItemKind
fastEye = ItemKind
  { isymbol  = toContentSymbol 'j'
  , iname    = "injective jaw"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3, 0), (4, 6), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 5, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Hungers but never eats. Bites but never swallows. Burrows its own image through, but never carries anything back."  -- rather weak: not about injective objects, but puny, concrete, injective functions  --- where's the madness in that?
  , ikit     = [ (S_TOOTH, COrgan), (S_LIP, COrgan)  -- at least one non-timed
               , (S_SPEED_GLAND_10, COrgan)
               , (S_VISION_6, COrgan), (S_EAR_3, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]
  }
nose :: ItemKind
nose = ItemKind  -- depends solely on smell
  { isymbol  = toContentSymbol 'n'
  , iname    = "point-free nose"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1), (MOBILE_MONSTER, 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(3, 0), (4, 5), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1)  -- can't project
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "No mouth, yet it devours everything around, constantly sniffing itself inward; pure movement structure, no constant point to focus one's maddened gaze on."
  , ikit     = [ (S_TIP, COrgan), (S_LIP, COrgan)  -- at least one non-timed
               , (S_NOSTRIL, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no sight nor hearing
  }
elbow :: ItemKind
elbow = ItemKind
  { isymbol  = toContentSymbol 'e'
  , iname    = "commutative elbow"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 0), (4, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An arm strung like a bow. A few edges, but none keen enough. A few points, but none piercing. Deadly objects zip out of the void."
  , ikit     = [ (S_SPEED_GLAND_5, COrgan), (S_BARK, COrgan)
               , (S_VISION_12, COrgan), (S_EAR_8, COrgan)
                   -- too powerful to get stronger sight
               , (S_SAPIENT_BRAIN, COrgan)
               , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
               , (WEAK_ARROW, CStash), (WEAK_ARROW, CStash) ]
  }
torsor :: ItemKind
torsor = ItemKind
  { isymbol  = toContentSymbol 'T'
  , iname    = "The Forgetful Torsor"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(9, 0), (10, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 300, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkAlter (-1)  -- can't exit the gated level; a boss,
                                        -- but can dig rubble, ice
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A principal homogeneous manifold, that acts freely and with enormous force, but whose stabilizers are trivial, making it rather helpless without a support group."
  , ikit     = [ (S_RIGHT_TORSION, COrgan), (S_LEFT_TORSION, COrgan)
               , (S_PUPIL, COrgan)
               , (S_TENTACLE, COrgan)  -- low timeout, so rarely a stall
               , (S_EAR_8, COrgan)
               , (S_SAPIENT_BRAIN, COrgan)
               ]
  }

-- * Cars

car :: ItemKind
car = ItemKind
  { isymbol  = toContentSymbol 't'
  , iname    = "car"
  , ifreq    = [(CAR, 100), (MOBILE, 1), (MOBILE_CAR, 100)]
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
  , iweight  = 7000000
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
  , iweight  = 8000000
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
  , iweight  = 6000000
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , ieffects = [ OnSmash $ Explode S_FIRECRACKER ] -- Batteries explode with colorful fire
  , idesc    = "New generation electric vehicle of questionable reputation"
  }

blackVolga :: ItemKind
blackVolga = car
  { isymbol  = toContentSymbol 'V'
  , iname    = "black Volga"
  , iflavour = zipPlain [BrBlack]
  , iweight  = 9000000
  , iaspects = [ AddSkill SkMaxHP 50, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-2)
               , SetFlag Durable ]
  , idesc    = "Mysterious and legendary vehicle, who knows what it itentions are?"
  }
