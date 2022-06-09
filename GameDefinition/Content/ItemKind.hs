-- | Definitions of basic items.
module Content.ItemKind
  ( pattern FOOD
  , pattern ARMOR_LOOSE
  , groupNamesSingleton
  , groupNames
  , content
  , items
  , otherItemContent
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Content.RuleKind

-- * Group name patterns

-- |List of singleton item group names
groupNamesSingleton :: [GroupName ItemKind]
groupNamesSingleton =
    [ S_FRAGRANCE
    , S_SINGLE_SPARK
    , S_SPARK
    , SODA_UNKNOWN
    , FOOD_UNKNOWN
    , PAPER_UNKNOWN
    , HAMMER_UNKNOWN
    , CURRENCY_UNKNOWN]
    ++ actorsGNSingleton ++ organsGNSingleton
    ++ blastsGNSingleton ++ temporariesGNSingleton

-- |List of item group names
groupNames :: [GroupName ItemKind]
groupNames =
    [ TREASURE
    , ANY_PAPER
    , ANY_GLASS
    , ANY_POTION
    , EXPLOSIVE
    , VALUABLE
    , UNREPORTED_INVENTORY
    , FOOD
    , ARMOR_LOOSE ]
    ++ embedsGN ++ actorsGN ++ blastsGN

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern SODA_UNKNOWN :: GroupName c
pattern SODA_UNKNOWN = GroupName "potion unknown"
pattern FOOD_UNKNOWN :: GroupName c
pattern FOOD_UNKNOWN = GroupName "edible plant unknown"
pattern PAPER_UNKNOWN :: GroupName c
pattern PAPER_UNKNOWN = GroupName "paper unknown"
pattern HAMMER_UNKNOWN :: GroupName c
pattern HAMMER_UNKNOWN = GroupName "hammer unknown"
pattern CURRENCY_UNKNOWN :: GroupName c
pattern CURRENCY_UNKNOWN = GroupName "currency unknown"

-- |Group of edible items
pattern FOOD :: GroupName c
pattern FOOD = GroupName "edible thing"
-- |Wearable clothes
pattern ARMOR_LOOSE :: GroupName c
pattern ARMOR_LOOSE = GroupName "loose armor"

-- * Content

-- |List of all things that are represented as items (`items` ++ `otherItemContent`)
content :: [ItemKind]
content = items ++ otherItemContent

-- |List of all things that are represented as items, but are not items (actors, organs...)
otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

-- |List of all items
items :: [ItemKind]
items =
    [ computerMouse
    , paralizingProj
    , diablotekSupply
    , iphoneBattery
    , flashBomb
    , swollenLithiumIon
    , sodaTemplate
    , soda1
    , soda2
    , iceTea
    , paperTemplate
    , paper1
    , paper2
    , paper3
    , foodTemplate
    , food1
    , food2
    , pwPullover
    , miniPullover
    , hatUshanka
    , miniBalaclava
    , miniJacket
    , laptopCharger
    , bigLaptopCharger
    , knife
    , pencil
    , sword
    , cableTray
    , currencyTemplate
    , currency ]

symbolProjectile :: ContentSymbol ItemKind
symbolProjectile = rsymbolProjectile $ ritemSymbols standardRules
_symbolLauncher :: ContentSymbol c
_symbolLauncher  = toContentSymbol '}'
symbolGold :: ContentSymbol c
symbolGold       = rsymbolGold $ ritemSymbols standardRules
symbolPotion :: ContentSymbol c
symbolPotion     = rsymbolPotion $ ritemSymbols standardRules
symbolPaper :: ContentSymbol c
symbolPaper     = rsymbolPaper $ ritemSymbols standardRules
symbolTorsoArmor :: ContentSymbol c
symbolTorsoArmor = rsymbolTorsoArmor $ ritemSymbols standardRules
symbolMiscArmor :: ContentSymbol c
symbolMiscArmor  = rsymbolMiscArmor $ ritemSymbols standardRules
symbolClothes :: ContentSymbol c
symbolClothes    = rsymbolClothes $ ritemSymbols standardRules
symbolPolearm :: ContentSymbol c
symbolPolearm    = rsymbolPolearm $ ritemSymbols standardRules
symbolEdged :: ContentSymbol c
symbolEdged      = rsymbolEdged $ ritemSymbols standardRules
symbolHafted :: ContentSymbol c
symbolHafted     = rsymbolHafted $ ritemSymbols standardRules
_symbolStaff :: ContentSymbol c
_symbolStaff     = toContentSymbol '_'
symbolFood :: ContentSymbol c
symbolFood       = rsymbolFood $ ritemSymbols standardRules

-- ** Thrown weapons

computerMouse :: ItemKind
computerMouse = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "computer mouse"
  , ifreq    = [ (S_COMPUTER_MOUSE, 1)
               , (UNREPORTED_INVENTORY, 1) ]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 + 1 `d` 2
  , irarity  = [(1, 20), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -16 * 5
               , SetFlag Fragile
               , toVelocity 50 ]
  , ieffects = []
  , idesc    = "Computer mouse that someone left unattended."
  , ikit     = []
  }

-- ** Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed aspects id by use.
-- This is an extremely large value for @Paralyze@. Normally for such values
-- we should instead use condition that disables (almost) all stats,
-- except @SkWait@, so that the player can switch leader and not be
-- helpless nor experience instadeath (unless his party is 1-person
-- or the actor is isolated, but that's usually player's fault).
paralizingProj :: ItemKind
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bolas set"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [Paralyze 15, Discharge 1 100]
  , idesc    = "Wood balls tied with hemp rope. The foe is unlikely to use its main weapon while fighting for balance."
  , ikit     = []
  }

-- ** Explosives, with the only effect being @Explode@

diablotekSupply :: ItemKind
diablotekSupply = ItemKind
  { isymbol  = '='
  , iname    = "diablotek power supply"
  , ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 200)]
  , iflavour = zipPlain [Red]
  , icount   = 1 `dL` 5
  , irarity  = [(5, 8), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 5000
  , idamage  = 0
  , iaspects = [ SetFlag Lobable, SetFlag Fragile ]
  , ieffects = [ Explode S_FOCUSED_FRAGMENTATION
               , OnSmash $ Explode S_VIOLENT_FRAGMENTATION ]
  , idesc    = "Particulary unstable power supply"
  , ikit     = []
  }
iphoneBattery :: ItemKind
iphoneBattery = diablotekSupply
  { iname    = "battery"
  , isymbol  = ','
  , iflavour = zipPlain [Magenta]
  , iverbHit = "flap"
  , iweight  = 50
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 200 ]
  , ieffects = [ Explode S_FOCUSED_CONCUSSION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
  , idesc    = "Particularly swollen, small battery"
  }
flashBomb :: ItemKind
flashBomb = diablotekSupply
  { iname    = "magnesium ribbon"
  , isymbol  = '+'
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "flash"
  , iweight  = 400
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]
  , ieffects = [Explode S_FOCUSED_FLASH, OnSmash (Explode S_VIOLENT_FLASH)]
  , idesc    = "Produces bright light"
  }
swollenLithiumIon :: ItemKind
swollenLithiumIon = diablotekSupply
  { iname = "battery"
  , isymbol = '|'
  , iflavour = zipPlain [BrMagenta]
  , irarity  = [(1, 5), (5, 6)]
  , iverbHit = "sizzle"
  , iweight  = 250
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [Explode S_LITHIUM_ION, OnSmash (Explode S_LITHIUM_ION)]
  , idesc    = "Large, swollen battery, looks dangerous"
  }

sodaTemplate :: ItemKind
sodaTemplate = ItemKind
  { isymbol  = symbolPotion
  , iname    = "soda can"
  , ifreq    = [(SODA_UNKNOWN, 1)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ PresentAs SODA_UNKNOWN, SetFlag Lobable, SetFlag Fragile, toVelocity 50 ]
  , ieffects = []
  , idesc    = "Soft drink can"
  , ikit     = []
  }
soda1 :: ItemKind
soda1 = sodaTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , ieffects = [ RefillHP 15, DropItem 1 maxBound COrgan S_POISONED
               , OnSmash (Explode S_HEALING_MIST) ]
  }
soda2 :: ItemKind
soda2 = sodaTemplate
  { iname    = "soda bottle"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 6), (10, 10)]
  , ieffects = [ RefillHP 25
               , DropItem maxBound maxBound COrgan CONDITION
               , OnSmash (Explode S_HEALING_MIST_2) ]
  , idesc    = "Soft drink bottle"
  }
iceTea :: ItemKind
iceTea = sodaTemplate
  { iname    = "ice tea bottle"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 6), (10, 10)]
  , idesc    = "Soft drink bottle"
  , ieffects = [ RefillCalm 30
               , DropItem maxBound maxBound COrgan CONDITION
               , OnSmash (Explode S_HEALING_MIST)]
  }

paperTemplate :: ItemKind
paperTemplate = ItemKind
  { isymbol  = symbolPaper
  , iname    = "paper"
  , ifreq    = [(PAPER_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain stdCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 14), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [ PresentAs PAPER_UNKNOWN
               , toVelocity 30 ]  -- bad shape, even rolled up
  , ieffects = []
  , idesc    = "Scraps of haphazardly scribbled mysteries from beyond. Is this equation an alchemical recipe? Is this diagram an extradimensional map? Is this formula a secret call sign?"
  , ikit     = []
  }
paper1 :: ItemKind
paper1 = paperTemplate
  { ifreq    = [(TREASURE, 100), (ANY_PAPER, 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so found early for a unique
  , iaspects = [ELabel "on monadic projects"]
  , ieffects = [Summon HERO 1]
  , idesc    = "Reading this paper aloud may summon people interested in the topic."
  }
paper2 :: ItemKind
paper2 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , iaspects = [ELabel "on anatomy"]
  , irarity  = [(1, 6), (10, 2)]
  , ieffects = [RefillHP 20]
  , idesc    = "Reading this paper may help you heal your wounds"
  }
paper3 :: ItemKind
paper3 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , iaspects = [ELabel "about some nonsense"]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Teleport 5, Paralyze 10, InsertMove 30
                      , Detect DetectEmbed 12, Detect DetectHidden 20 ]]
  , idesc    = "This paper looks like mostly nonsense. Who knows what might happen when you read it?"
  }

foodTemplate :: ItemKind
foodTemplate = ItemKind
  { isymbol  = symbolFood
  , iname    = "food"
  , ifreq    = [(FOOD_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol
  , icount   = 1 `dL` 5
  , irarity  = [(1, 12), (10, 6)]
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [ PresentAs FOOD_UNKNOWN
               , toVelocity 30 ]  -- low density, often falling apart
  , ieffects = []
  , idesc    = "Withered but fragrant bits of a colorful plant. Taste tolerably and break down easily, but only eating may reveal the full effects."
  , ikit     = []
  }
food1 :: ItemKind
food1 = foodTemplate
  { iname    = "candy bar"
  , ifreq    = [(COMMON_ITEM, 100), (FOOD, 100)]
  , ieffects = [RefillHP 1, toOrganBad S_IMMOBILE (5 + 1 `d` 5)]
  }
food2 :: ItemKind
food2 = foodTemplate
  { iname    = "sandwich"
  , ifreq    = [(COMMON_ITEM, 100), (FOOD, 100)]
  , ieffects = [RefillHP 5, toOrganBad S_IMMOBILE (2 + 1 `d` 3)]
  }

-- ** Armor

pwPullover :: ItemKind
pwPullover = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "pw pullover"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_LOOSE, 1), (STARTING_ARMOR, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 9), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 200
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-2)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = []
  , idesc    = "Nice pullover with letters PW"
  , ikit     = []
  }
miniPullover :: ItemKind
miniPullover = pwPullover
  { iname    = "mini pullover"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_LOOSE, 1), (ARMOR_RANGED, 50)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-3)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (4 + 1 `dL` 2) * 3
               , AddSkill SkOdor 2
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "Stylish pullover with orbits and a planet"
  }
hatUshanka :: ItemKind
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (ARMOR_LOOSE, 1)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 6), (10, 1)]
  , iverbHit = "tickle"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 3
               , AddSkill SkArmorMelee 5, AddSkill SkHearing (-6)
               , SetFlag Periodic, SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = [RefillCalm 1]
  , idesc    = "Soft and warm fur. It keeps your ears warm."
  , ikit     = []
  }
miniBalaclava :: ItemKind
miniBalaclava = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "mini balaclava"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (STARTING_ARMOR, 50)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "bounce"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (2 + 1 `dL` 2) * 3  -- headshot
               , AddSkill SkSight (-2)
               , AddSkill SkHearing (-3), AddSkill SkSmell (-5)
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "Blocks out everything, including your senses."
  , ikit     = []
  }
miniJacket :: ItemKind
miniJacket = ItemKind
  { isymbol  = symbolClothes
  , iname    = "mini jacket"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_LOOSE, 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "stroke"
  , iweight  = 5000
  , idamage  = 0
  , iaspects = [ Timeout $ (1 `d` 2) * 3
               , AddSkill SkSpeed 2
               , AddSkill SkOdor 2
               , SetFlag Periodic, SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotSpeed ]
  , ieffects = [RefillCalm 1]
  , idesc    = "Jacket embellished with a planet and orbits"
  , ikit     = []
  }

knife :: ItemKind
knife = ItemKind
  { isymbol  = symbolEdged
  , iname    = "pocket knife"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 150)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(2, 45), (4, 1)]
  , iverbHit = "cut"
  , iweight  = 800
  , idamage  = 6 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorMelee $ (1 `d` 2) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]
  , ieffects = []
  , idesc    = "Typical pocket knife, nothing unusual."
  , ikit     = []
  }

pencil :: ItemKind
pencil = ItemKind
  { isymbol  = symbolEdged
  , iname    = "pencil"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 200)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(2, 45), (4, 1)]
  , iverbHit = "cut"
  , iweight  = 800
  , idamage  = 6 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorMelee $ (1 `d` 2) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]
  , ieffects = []
  , idesc    = "Very sharp pencil"
  , ikit     = []
  }

laptopCharger :: ItemKind
laptopCharger = ItemKind
  { isymbol  = symbolHafted
  , iname    = "laptop charger"
  , ifreq    = [(HAMMER_UNKNOWN, 1)]
  , iflavour = zipFancy [BrBlack]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(3, 25), (5, 1)]
  , iverbHit = "swing"
  , iweight  = 150
  , idamage  = 8 `d` 1  , iaspects = [ PresentAs HAMMER_UNKNOWN
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 40 ]
  , ieffects = []
  , idesc    = "Regular laptop charger with a cord, can be swung at enemies."
  , ikit     = []
  }
bigLaptopCharger :: ItemKind
bigLaptopCharger = laptopCharger
  { ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 70)]
  , iaspects = [Timeout 5, EqpSlot EqpSlotWeaponBig]
               ++ iaspects laptopCharger
  , idesc    = "Big, 200W laptop charger. Probably for a gaming laptop."
  , iweight  = 300
  }
sword :: ItemKind
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 30)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (6, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = 10 `d` 1
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = []
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
cableTray :: ItemKind
cableTray = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "cable tray"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 20)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(5, 1), (8, 12)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 10
               , AddSkill SkHurtMelee $ (-5 + 1 `dL` 3) * 5
               , AddSkill SkArmorMelee 20
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]
  , ieffects = []
  , idesc    = "Long, aluminium cable tray. Can be used as a weapon."
  , ikit     = []
  }

currencyTemplate :: ItemKind
currencyTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "ECTS point"
  , ifreq    = [(CURRENCY_UNKNOWN, 1), (VALUABLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , idamage  = 0
  , iaspects = [PresentAs CURRENCY_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Everything you might need for a successful career"
  , ikit     = []
  }
currency :: ItemKind
currency = currencyTemplate
  { ifreq    = [(TREASURE, 100), (S_CURRENCY, 100), (VALUABLE, 1)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
               ++ iaspects currencyTemplate
  }
