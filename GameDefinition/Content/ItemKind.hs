-- | Definitions of basic items.
module Content.ItemKind
  ( pattern HARPOON
  , pattern FOOD
  , pattern RING_OF_OPPORTUNITY_GRENADIER
  , pattern ARMOR_LOOSE
  , pattern CLOTHING_MISC
  , pattern CHIC_GEAR
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

groupNamesSingleton :: [GroupName ItemKind]
groupNamesSingleton =
    [ S_FRAGRANCE
    , S_SINGLE_SPARK
    , S_SPARK
    , SODA_UNKNOWN
    , FOOD_UNKNOWN
    , PAPER_UNKNOWN
    , NECKLACE_UNKNOWN
    , RING_UNKNOWN
    , HAMMER_UNKNOWN
    , CURRENCY_UNKNOWN]
    ++ actorsGNSingleton ++ organsGNSingleton
    ++ blastsGNSingleton ++ temporariesGNSingleton

groupNames :: [GroupName ItemKind]
groupNames =
    [ TREASURE
    , ANY_PAPER
    , ANY_GLASS
    , ANY_POTION
    , EXPLOSIVE
    , ANY_JEWELRY
    , VALUABLE
    , UNREPORTED_INVENTORY
    , HARPOON
    , FOOD
    , RING_OF_OPPORTUNITY_GRENADIER
    , ARMOR_LOOSE
    , CLOTHING_MISC
    , CHIC_GEAR]
    ++ embedsGN ++ actorsGN ++ blastsGN

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern SODA_UNKNOWN :: GroupName c
pattern SODA_UNKNOWN = GroupName "potion unknown"
pattern FOOD_UNKNOWN :: GroupName c
pattern FOOD_UNKNOWN = GroupName "edible plant unknown"
pattern PAPER_UNKNOWN :: GroupName c
pattern PAPER_UNKNOWN = GroupName "paper unknown"
pattern NECKLACE_UNKNOWN :: GroupName c
pattern NECKLACE_UNKNOWN = GroupName "necklace unknown"
pattern RING_UNKNOWN :: GroupName c
pattern RING_UNKNOWN = GroupName "ring unknown"
pattern HAMMER_UNKNOWN :: GroupName c
pattern HAMMER_UNKNOWN = GroupName "hammer unknown"
pattern CURRENCY_UNKNOWN :: GroupName c
pattern CURRENCY_UNKNOWN = GroupName "currency unknown"

pattern HARPOON :: GroupName c
pattern HARPOON = GroupName "harpoon"
pattern FOOD :: GroupName c
pattern FOOD = GroupName "edible plant"
pattern RING_OF_OPPORTUNITY_GRENADIER :: GroupName c
pattern RING_OF_OPPORTUNITY_GRENADIER = GroupName "ring of grenadier"
pattern ARMOR_LOOSE :: GroupName c
pattern ARMOR_LOOSE = GroupName "loose armor"
pattern CLOTHING_MISC :: GroupName c
pattern CLOTHING_MISC = GroupName "miscellaneous clothing"
pattern CHIC_GEAR :: GroupName c
pattern CHIC_GEAR = GroupName "chic gear"

-- * Content

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
    [ sandstoneRock
    , dart
    , spike
    , spike2
    , slingStone
    , slingBullet
    , paralizingProj
    , harpoon
    , harpoon2
    , net
    , diablotekSupply
    , concussionBomb
    , flashBomb
    , firecrackerBomb
    , sodaTemplate
    , soda1
    , soda2
    , iceTea
    , paperTemplate
    , paper1
    , paper2
    , paper3
    , paper4
    , paper5
    , paper6
    , paper7
    , paper8
    , paper9
    , paper10
    , paper11
    , paper12
    , paper13
    , foodTemplate
    , food1
    , food2
    , light1
    , light2
    , light3
    , necklaceTemplate
    , necklace1
    , necklace2
    , necklace3
    , necklace4
    , necklace5
    , necklace6
    , necklace7
    , necklace8
    , necklace9
    , necklace10
    , motionScanner
    , imageItensifier
    , sightSharpening
    , ringTemplate
    , ring1
    , ring2
    , ring3
    , ring4
    , ring5
    , ring6
    , ring7
    , ring8
    , ring9
    , ring10
    , pwPullover
    , miniPullover
    , gloveFencing
    , hatUshanka
    , capReinforced
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

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has multiple random aspects.
-- Instead multiply dice results, e.g., (1 `d` 3) * 5 instead of 1 `d` 15.
--
-- Beware of non-periodic non-weapon durable items with beneficial effects
-- and low timeout -- AI will starve applying such an item incessantly.

-- * Item group symbols, partially from Nethack

symbolProjectile :: ContentSymbol ItemKind
symbolProjectile = rsymbolProjectile $ ritemSymbols standardRules
_symbolLauncher :: ContentSymbol c
_symbolLauncher  = toContentSymbol '}'
symbolLight :: ContentSymbol c
symbolLight      = rsymbolLight $ ritemSymbols standardRules
symbolGold :: ContentSymbol c
symbolGold       = rsymbolGold $ ritemSymbols standardRules
symbolNecklace :: ContentSymbol c
symbolNecklace   = rsymbolNecklace $ ritemSymbols standardRules
symbolRing :: ContentSymbol c
symbolRing       = rsymbolRing $ ritemSymbols standardRules
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

sandstoneRock :: ItemKind
sandstoneRock = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sandstone rock"
  , ifreq    = [ (S_SANDSTONE_ROCK, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `d` 2  -- > 1, to let AI ignore sole pieces
  , irarity  = [(1, 20), (10, 1)]  -- a few already in starting stash
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -16 * 5
               , SetFlag Fragile
               , toVelocity 70 ] -- not dense, irregular
  , ieffects = []
  , idesc    = "A lump of brittle sandstone rock."
  , ikit     = []
  }
dart :: ItemKind
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 15), (10, 5)]
  , iverbHit = "prick"
  , iweight  = 40
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-15 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- only good against leather
  , ieffects = []
  , idesc    = "A sharp delicate dart with fins."
  , ikit     = []
  }
spike :: ItemKind
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "spike"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 10), (10, 8)]
  , iverbHit = "nick"
  , iweight  = 150
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy vs armor
               , SetFlag MinorEffects
               , toVelocity 70 ]  -- hitting with tip costs speed
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
      -- this results in a wordy item synopsis, but it's OK, the spark really
      -- is useful in some situations, not just a flavour
  , idesc    = "A cruel long nail with small head."  -- "Much inferior to arrows though, especially given the contravariance problems."  -- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
spike2 :: ItemKind
spike2 = spike
  { ifreq    = [(COMMON_ITEM, 2), (ANY_ARROW, 1), (WEAK_ARROW, 1)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "penetrate"
  , iweight  = 200
  , idamage  = 4 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag MinorEffects
               , Odds (10 * 1 `dL` 10) [] [toVelocity 70] ]
                   -- at deep levels sometimes even don't limit velocity
  , idesc    = "A jagged skewer of rusty metal."
  }
slingStone :: ItemKind
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling stone"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 3 `dL` 4
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "batter"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy, to bludgeon through armor
               , SetFlag MinorEffects
               , toVelocity 150 ]
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
  , idesc    = "A round stone, carefully sized and smoothed to fit the pouch of a standard string and cloth sling."
  , ikit     = []
  }
slingBullet :: ItemKind
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling bullet"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1 + 6 `dL` 4
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "slug"
  , iweight  = 28
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- not too good against armor
               , ToThrow $ ThrowMod 200 100 2  -- piercing
               , SetFlag Fragile ]
                   -- otherwise would rarely break and the player would have
                   -- unlimited resource and would have to pick up constantly
  , ieffects = []
  , idesc    = "Small almond-shaped leaden projectile that weighs more than the sling used to tie the bag. It doesn't drop out of the sling's pouch when swung and doesn't snag when released. Known to pierce through flesh, at least at maximum speed."  -- we lie, it doesn't slow down in our model; but it stops piercing alright
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
harpoon :: ItemKind
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [(COMMON_ITEM, 100), (HARPOON, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 10)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = 5 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
  , ieffects = [ PullActor (ThrowMod 200 50 1)  -- 1 step, fast
               , Yell ]  -- yell, because brutal
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
harpoon2 :: ItemKind
harpoon2 = harpoon
  { iname    = "The whaling Harpoon"
  , ifreq    = [(COMMON_ITEM, 10), (HARPOON, 2)]
  , icount   = 2 `dL` 5
  , iweight  = 1000
  , idamage  = 21 `d` 1
  , iaspects = SetFlag Unique : delete (SetFlag Durable) (iaspects harpoon)
  , idesc    = "With a brittle, barbed head and thick cord, this ancient weapon is designed for formidable prey. The age has made the edge thinner and sharper, but brittle and splintering, so it won't last beyond a single hit. "
  }
net :: ItemKind
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 3
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = 2 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [ toOrganBad S_SLOWED (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp ARMOR_LOOSE
                   -- only one of each kind is dropped, because no rubbish
                   -- in this group and so no risk of exploit
               , SendFlying (ThrowMod 100 50 1) ]  -- 1 step; painful
  , idesc    = "A wide net with weights along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- ** Explosives, with the only effect being @Explode@

diablotekSupply :: ItemKind
diablotekSupply = ItemKind
  { isymbol  = symbolProjectile
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
  , idesc    = "A particulary unstable power supply"
  , ikit     = []
  }
concussionBomb :: ItemKind
concussionBomb = diablotekSupply
  { iname    = "satchel"
      -- slightly stabilized nitroglycerine in a soft satchel, hence
      -- no fragmentation, but huge shock wave despite small size and lack of
      -- strong container to build up pressure (hence only mild hearing loss);
      -- indoors helps the shock wave; unstable enough that no fuze required
  , iflavour = zipPlain [Magenta]
  , iverbHit = "flap"
  , iweight  = 400
  , iaspects = [ ELabel "of mining charges"
               , SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- flappy and so slow
  , ieffects = [ Explode S_FOCUSED_CONCUSSION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
  , idesc    = "Avoid sudden movements."
  }
-- Not flashbang, because powerful bang without fragmentation is harder
-- to manufacture (requires an oxidizer and steel canister with holes).
-- The bang would also paralyze and/or lower the movement skill
-- (out of balance due to ear trauma).
flashBomb :: ItemKind
flashBomb = diablotekSupply
  { iname    = "magnesium ribbon"  -- filled with magnesium flash powder
  , iflavour = zipPlain [BrYellow]  -- avoid @BrWhite@; looks wrong in dark
  , iverbHit = "flash"
  , iweight  = 400
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- bad shape for throwing
  , ieffects = [Explode S_FOCUSED_FLASH, OnSmash (Explode S_VIOLENT_FLASH)]
  , idesc    = "For dramatic entrances and urgent exits."
  }
firecrackerBomb :: ItemKind
firecrackerBomb = diablotekSupply
  { iname = "roll"  -- not fireworks, as they require outdoors
  , iflavour = zipPlain [BrMagenta]
  , irarity  = [(1, 5), (5, 6)]  -- a toy, if harmful
  , iverbHit = "crack"  -- a pun, matches the verb from "ItemKindBlast"
  , iweight  = 1000
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [Explode S_FIRECRACKER, OnSmash (Explode S_FIRECRACKER)]
  , idesc    = "String and paper, concealing a deadly surprise."
  }

-- ** Exploding consumables.

-- Not identified, because they are perfect for the id-by-use fun,
-- due to effects. They are fragile and upon hitting the ground explode
-- for effects roughly corresponding to their normal effects.
-- Whether to hit with them or explode them close to the target
-- is intended to be an interesting tactical decision.

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
  , idesc    = "A soft drink can"
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
  , idesc    = "A soft drink bottle"
  }
iceTea :: ItemKind
iceTea = sodaTemplate
  { iname    = "ice tea bottle"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 6), (10, 10)]
  , idesc    = "A soft drink bottle"
  , ieffects = [ RefillCalm 30
               , DropItem maxBound maxBound COrgan CONDITION
               , OnSmash (Explode S_HEALING_MIST)]
  }

-- ** Non-exploding consumables, not specifically designed for throwing

-- Readable or otherwise communicating consumables require high apply skill
-- to be consumed.

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
  { iname    = "the Paper"
  , ifreq    = [(TREASURE, 100), (ANY_PAPER, 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so found early for a unique
  , iaspects = [SetFlag Unique, ELabel "of Reckless Beacon"]
               ++ iaspects paperTemplate
  , ieffects = [Summon HERO 1]
  , idesc    = "The bright flame and sweet-smelling smoke of this heavily infused paper should attract natural creatures inhabiting the area, including human survivors, if any."
  }
paper2 :: ItemKind
paper2 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(1, 6), (10, 2)]
  , ieffects = [Ascend False]
  }
paper3 :: ItemKind
paper3 = paperTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Teleport 5, Paralyze 10, InsertMove 30
                      , Detect DetectEmbed 12, Detect DetectHidden 20 ]]
  }
paper4 :: ItemKind
paper4 = paperTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 14)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , OneOf [Summon HERO 1]
                           -- gaining a hero particularly uncommon
                       , Detect DetectLoot 20  -- the most useful of detections
                       , CreateItem Nothing CGround COMMON_ITEM timerNone ] ]
  }
paper5 :: ItemKind
paper5 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(1, 6)]  -- powerful, but low counts at the depths it appears on
  , ieffects = [InsertMove $ 20 + 1 `dL` 20]
  }
paper6 :: ItemKind
paper6 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(10, 11)]
  , ieffects = [PullActor (ThrowMod 800 75 1)]  -- 6 steps, 1.5 turns
  }
paper7 :: ItemKind
paper7 = paperTemplate
  { iname    = "the Paper"
  , ifreq    = [(TREASURE, 100), (ANY_PAPER, 100)]
  , icount   = 1
  , irarity  = [(10, 12)]
  , iaspects = [SetFlag Unique, ELabel "of Rescue Proclamation"]
               ++ iaspects paperTemplate
  , ieffects = [Summon HERO 1]
  , idesc    = "A survivor of past exploration missions is found that enjoys, apparently, complete physiological integrity. We can pronounce him a comrade in arms and let him join our party."
  }
paper8 :: ItemKind
paper8 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(10, 4)]  -- powerful, even if not ideal; scares newbies
  , ieffects = [Detect DetectAll 20]
  }
paper9 :: ItemKind
paper9 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , iaspects = ELabel "of cue interpretation"
               : iaspects paperTemplate
  , ieffects = [Detect DetectActor 20]
  }
paper10 :: ItemKind
paper10 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 20)]  -- uncommon deep down, where all is known
  , iaspects = ELabel "of scientific explanation"
               : iaspects paperTemplate
  , ieffects = [Identify `AndEffect` RefillCalm 10]
  , idesc    = "The most pressing existential concerns are met with a deeply satisfying scientific answer."
  }
paper11 :: ItemKind
paper11 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(10, 20)]  -- at gameover a crucial item may be missing
  , iaspects = ELabel "of transmutation"
               : iaspects paperTemplate
  , ieffects = [PolyItem `AndEffect` Explode S_FIRECRACKER]
  }
paper12 :: ItemKind
paper12 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of transfiguration"
               : iaspects paperTemplate
  , ieffects = [RerollItem]
  }
paper13 :: ItemKind
paper13 = paperTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_PAPER, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of similarity"
               : iaspects paperTemplate
  , ieffects = [DupItem]
  }

-- Foods require only minimal apply skill to consume.

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

-- ** Lights

light1 :: ItemKind
light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [ (COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)
               , (S_WOODEN_TORCH, 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 4
  , irarity  = [(1, 40), (4, 1)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 3  -- no malus, to lessen micromanagement
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
                   -- not Fragile; reusable flare
  , ieffects = [Burn 1]
  , idesc    = "A heavy smoking wooden torch, improvised using a cloth soaked in tar, burning in an unsteady glow."
  , ikit     = []
  }
light2 :: ItemKind
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [(COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 `dL` 2
  , irarity  = [(4, 10)]
  , iverbHit = "burn"
  , iweight  = 1500
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkShine 3
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Explode S_FOCUSED_BURNING_OIL_2
               , OnSmash (Explode S_VIOLENT_BURNING_OIL_2) ]
  , idesc    = "A small clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
light3 :: ItemKind
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [(COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(10, 6)]
  , iverbHit = "burn"
  , iweight  = 3000
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkShine 4
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Explode S_FOCUSED_BURNING_OIL_4
               , OnSmash (Explode S_VIOLENT_BURNING_OIL_4) ]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }

-- ** Periodic jewelry

-- Morally these are the aspects, but we also need to add a fake @Timeout@,
-- to let clients know that the not identified item is periodic jewelry.
iaspects_necklaceTemplate :: [Aspect]
iaspects_necklaceTemplate =
  [ PresentAs NECKLACE_UNKNOWN
  , SetFlag Periodic, SetFlag Precious, SetFlag Equipable
  , toVelocity 50 ]  -- not dense enough
-- Not identified, because id by use, e.g., via periodic activations. Fun.
necklaceTemplate :: ItemKind
necklaceTemplate = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [(NECKLACE_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(4, 3), (10, 6)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = 0
  , iaspects = Timeout 1000000
                 -- fake, needed to display "charging"; the timeout itself
                 -- won't be displayed thanks to periodic; as a side-effect,
                 -- it can't be activated until identified, which is better
                 -- than letting the player try to activate before the real
                 -- cooldown is over and waste turn
               : iaspects_necklaceTemplate
  , ieffects = []
  , idesc    = "Menacing Greek symbols shimmer with increasing speed along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 :: ItemKind
necklace1 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 3)]
  , iaspects = [ SetFlag Unique, ELabel "of Aromata"
               , Timeout $ (4 - 1 `dL` 3) * 10
                   -- priceless, so worth the long wait and Calm drain
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ RefillCalm (-5)
               , When (TriggeredBy ActivationPeriodic) $ RefillHP 1 ]
  , idesc    = "A cord of freshly dried herbs and healing berries."
  }
necklace2 :: ItemKind
necklace2 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
      -- too nasty to call it just a COMMON_ITEM
  , irarity  = [(10, 3)]
  , iaspects = [ SetFlag Unique, ELabel "of Live Bait"
               , Timeout 30
               , AddSkill SkOdor 2
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ DropItem 1 1 COrgan CONDITION  -- mildly useful when applied
               , When (TriggeredBy ActivationPeriodic) $ SeqEffect
                   [ Impress
                   , Explode S_WASTE ] ]
  , idesc    = "A cord hung with lumps of decaying meat. It's better not to think about the source."
  }
necklace3 :: ItemKind
necklace3 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of fearful listening"
               , Timeout 40
                   -- has to be larger than Calm drain or item not removable;
                   -- equal is not enough if enemies drained Calm already
               , AddSkill SkHearing 6 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectActor 20  -- can be applied; destroys the item
               , When (TriggeredBy ActivationPeriodic) $ RefillCalm (-30) ]
  }
necklace4 :: ItemKind
necklace4 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of escape"
               , Timeout $ (7 - 1 `dL` 5) * 10 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Teleport $ 14 + 3 `d` 3  -- can be applied; destroys the item
               , Detect DetectExit 20
               , Yell ]  -- drawback when used for quick exploring
  , idesc    = "A supple chain that slips through your fingers."
  }
necklace5 :: ItemKind
necklace5 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of greed"
               , Timeout ((2 + 1 `d` 3) * 10) ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectLoot 20
               , toOrganBad S_PARSIMONIOUS (5 + 1 `d` 3)  -- hard to flee
               , When (TriggeredBy ActivationPeriodic) $ Teleport 40 ]  -- risky
  }
necklace6 :: ItemKind
necklace6 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((3 + 1 `d` 3 - 1 `dL` 3) * 2)
               : iaspects_necklaceTemplate  -- OP if Durable; free blink
  , ieffects = [Teleport $ 3 `d` 2]
  }
necklace7 :: ItemKind
necklace7 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((1 `d` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
                  -- the @50@ is only for the case of very light actor, etc.
  }
necklace8 :: ItemKind
necklace8 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 1)]  -- different gameplay for the actor that wears it
  , iaspects = [ SetFlag Unique, ELabel "of Overdrive"
               , Timeout 4
               , AddSkill SkMaxHP 25  -- give incentive to cope with impatience
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ InsertMove $ 9 + 1 `d` 11  -- unpredictable
               , toOrganBad S_IMPATIENT 4]
                 -- The same duration as timeout, to avoid spurious messages
                 -- as well as unlimited accumulation of the duration.
  , idesc    = "A string of beads in various colours, with no discernable pattern."
  }
necklace9 :: ItemKind
necklace9 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(4, 3)]  -- entirely optional
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)  -- low timeout for offensive use
               : iaspects_necklaceTemplate
  , ieffects = [Explode S_SPARK]
  }
necklace10 :: ItemKind
necklace10 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((3 + 1 `d` 3) * 10)
               : iaspects_necklaceTemplate
                   -- high timeout to prevent spam obscuring messages
                   -- when other actors act and annoying bumping into
                   -- projectiles caused by own necklace when walking
  , ieffects = [Explode S_FRAGRANCE]
  }
motionScanner :: ItemKind
motionScanner = necklaceTemplate
  { iname    = "draft detector"
  , ifreq    = [(COMMON_ITEM, 100), (ADD_NOCTO_1, 20)]
  , irarity  = [(5, 2)]
  , iverbHit = "jingle"
  , iweight  = 300  -- almost gives it away
  , iaspects = [ Timeout $ 4 + 1 `dL` 6
                   -- positive dL dice, since the periodic effect is detrimental
               , AddSkill SkNocto 1
               , AddSkill SkArmorMelee $ (-4 + 1 `dL` 3) * 5
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects_necklaceTemplate
  , ieffects = [Explode S_PING_PLASH]
  , idesc    = "A silk flag with a bell for detecting sudden draft changes. May indicate a nearby corridor crossing or a fast enemy approaching in the dark. The bell is very noisy and casts light reflection flashes."
  }

-- ** Non-periodic jewelry

imageItensifier :: ItemKind
imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "light cone"
  , ifreq    = [(TREASURE, 100), (ADD_NOCTO_1, 80)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "bang"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1
               , AddSkill SkArmorMelee $ (-6 + 1 `dL` 3) * 5
               , SetFlag Precious, SetFlag Equipable
               , EqpSlot EqpSlotMiscBonus ]
  , ieffects = []
  , idesc    = "Contraption of lenses and mirrors on a polished brass headband for capturing and strengthening light in dark environment. Hampers vision in daylight. Stackable."
  , ikit     = []
  }
sightSharpening :: ItemKind
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "sharp monocle"
  , ifreq    = [(TREASURE, 20), (ADD_SIGHT, 1)]
      -- it's has to be very rare, because it's powerful and not unique,
      -- and also because it looks exactly as one of necklaces, so it would
      -- be misleading when seen on the map
  , irarity  = [(7, 1), (10, 12)]  -- low @ifreq@
  , iweight  = 50  -- heavier that it looks, due to glass
  , iaspects = [ AddSkill SkSight $ 1 + 1 `dL` 2
               , AddSkill SkHurtMelee $ (1 `d` 3) * 3
               , EqpSlot EqpSlotSight ]
               ++ iaspects ringTemplate
  , idesc    = "Lets you better focus your weaker eye."
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and stash or even activating a wrong item by mistake.
--
-- By general mechanisms, due to not having effects that could identify
-- them by observing the effect, rings are identified on pickup.
-- That's unlike necklaces, which provide the fun of id-by-use, because they
-- have effects and when the effects are triggered, they get identified.
ringTemplate :: ItemKind
ringTemplate = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [(RING_UNKNOWN, 1)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 2)]  -- the default very low
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = 0
  , iaspects = [PresentAs RING_UNKNOWN, SetFlag Precious, SetFlag Equipable]
  , ieffects = []
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural qualities and subtracting from others."
  , ikit     = []
  }
ring1 :: ItemKind
ring1 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(8, 4)]
  , iaspects = [ AddSkill SkSpeed $ 1 `dL` 2
               , AddSkill SkMaxHP (-20)
               , EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  }
ring2 :: ItemKind
ring2 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(8, 4)]
  , iaspects = [ AddSkill SkSpeed $ 1 + 1 `dL` 3
               , AddSkill SkArmorMelee (-40)
               , EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  }
ring3 :: ItemKind
ring3 = ringTemplate
  { iname    = "the Ring"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ SetFlag Unique, ELabel "of Rush"
               , AddSkill SkSpeed $ (1 + 1 `dL` 2) * 2
               , AddSkill SkMaxHP (-20)
               , AddSkill SkArmorMelee (-20)
               , SetFlag Durable, EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  , idesc    = "Roughly-shaped metal with shallow scratches marking it."
  }
ring4 :: ItemKind
ring4 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(3, 4), (10, 8)]
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 3 + (1 `dL` 2) * 2 ) * 3
               , AddSkill SkMaxHP (-10)
               , EqpSlot EqpSlotHurtMelee ]
               ++ iaspects ringTemplate
  }
ring5 :: ItemKind
ring5 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ AddSkill SkHurtMelee $ (4 + 1 `d` 3 + (1 `dL` 2) * 2 ) * 3
               , AddSkill SkArmorMelee (-20)
               , EqpSlot EqpSlotHurtMelee ]
               ++ iaspects ringTemplate
  }
ring6 :: ItemKind
ring6 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 8)]
  , iaspects = [ AddSkill SkMaxHP $ 5 + (1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkMaxCalm $ -30 + (1 `dL` 3) * 5
               , EqpSlot EqpSlotMaxHP ]
               ++ iaspects ringTemplate
  }
ring7 :: ItemKind
ring7 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(5, 1), (10, 9)]  -- needed after other items drop Calm
  , iaspects = [ AddSkill SkMaxCalm $ 30 + (1 `dL` 4) * 5
               , AddSkill SkHearing 6
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects ringTemplate
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring8 :: ItemKind
ring8 = ringTemplate  -- weak skill per eqp slot, so can be without drawbacks
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 3)]
  , iaspects = [ AddSkill SkShine 1
               , EqpSlot EqpSlotShine ]
               ++ iaspects ringTemplate
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring9 :: ItemKind
ring9 = ringTemplate
  { ifreq    = [(RING_OF_OPPORTUNITY_SNIPER, 1) ]  -- only for scenarios
  , irarity  = [(1, 1)]
  , iaspects = [ ELabel "of opportunity sniper"
               , AddSkill SkProject 8
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  }
ring10 :: ItemKind
ring10 = ringTemplate
  { ifreq    = [(RING_OF_OPPORTUNITY_GRENADIER, 1) ]  -- only for scenarios
  , irarity  = [(1, 1)]
  , iaspects = [ ELabel "of opportunity grenadier"
               , AddSkill SkProject 11
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
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
  , idesc    = "A nice pullover with letters PW"
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
  , idesc    = "A stylish pullover with orbits and a planet"
  }
gloveFencing :: ItemKind
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "winter glove"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (ARMOR_RANGED, 50)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorRanged $ (1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = []
  , idesc    = "A glove perfectly fit for cold winters in Warsaw"
  , ikit     = []
  }
hatUshanka :: ItemKind
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (CLOTHING_MISC, 1)
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
capReinforced :: ItemKind
capReinforced = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather cap"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (STARTING_ARMOR, 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 `d` 2) * 5
               , AddSkill SkProject 1
                   -- the brim shields against blinding by light sources, etc.
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotProject ]
  , ieffects = []
  , idesc    = "Boiled leather with a wide brim. It might soften a blow."
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
  , ifreq    = [(COMMON_ITEM, 100), (CLOTHING_MISC, 1), (CHIC_GEAR, 100)]
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
  , idesc    = "A jacket embellished with a planet and orbits"
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
  , idesc    = "A typical pocket knife, nothing unusual."
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
  , idesc    = "A very sharp pencil"
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
  , idesc    = "A long, aluminium cable tray. Can be used as a weapon."
  , ikit     = []
  }

-- ** Treasure

currencyTemplate :: ItemKind
currencyTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [(CURRENCY_UNKNOWN, 1), (VALUABLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , idamage  = 0
  , iaspects = [PresentAs CURRENCY_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
currency :: ItemKind
currency = currencyTemplate
  { ifreq    = [(TREASURE, 100), (S_CURRENCY, 100), (VALUABLE, 1)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
               ++ iaspects currencyTemplate
  }
