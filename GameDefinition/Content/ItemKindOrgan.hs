{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( pattern S_TIRE
  , pattern S_EXHAUST_FAN
  , pattern S_FIST
  , pattern S_FOOT
  , pattern S_SAPIENT_BRAIN
  , pattern S_CAR_COMPUTER
  , pattern S_CPU
  , pattern S_CAMERA
  , pattern S_EYE_6
  , pattern S_EAR_6
  , organsGNSingleton
  , organs
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

-- |Group name patterns
organsGNSingleton :: [GroupName ItemKind]
organsGNSingleton =
    [ S_TIRE
    , S_EXHAUST_FAN
    , S_FIST
    , S_FOOT
    , S_ARMORED_SKIN
    , S_SAPIENT_BRAIN
    , S_CAR_COMPUTER
    , S_CPU
    , S_CAMERA
    , S_EYE_6
    , S_EAR_6 ]

-- |Template used for some other organs
pattern S_ARMORED_SKIN :: GroupName c
pattern S_ARMORED_SKIN = GroupName "armored skin"
-- |Car tire used by cars as a weapon
pattern S_TIRE :: GroupName c
pattern S_TIRE = GroupName "tire"
-- |Exhaust fan used by computers as a weapon
pattern S_EXHAUST_FAN :: GroupName c
pattern S_EXHAUST_FAN = GroupName "exhaust fan"
-- |Fist used by humans
pattern S_FIST :: GroupName c
pattern S_FIST = GroupName "fist"
-- |Foot used by humans
pattern S_FOOT :: GroupName c
pattern S_FOOT = GroupName "foot"
-- |Brain used by humans
pattern S_SAPIENT_BRAIN :: GroupName c
pattern S_SAPIENT_BRAIN = GroupName "sapient brain"
-- |Computer used by cars as a brain
pattern S_CAR_COMPUTER :: GroupName c
pattern S_CAR_COMPUTER = GroupName "car computer"
-- |CPU used by computers as a brain
pattern S_CPU :: GroupName c
pattern S_CPU = GroupName "cpu"
-- |Camera used by cars and computers as eyes
pattern S_CAMERA :: GroupName c
pattern S_CAMERA = GroupName "camera"
-- |Default eye, used by e.g. humans
pattern S_EYE_6 :: GroupName c
pattern S_EYE_6 = GroupName "eye 6"
-- |Default ear, used by e.g. humans
pattern S_EAR_6 :: GroupName c
pattern S_EAR_6 = GroupName "ear 6"

-- * Content

-- |List of all organs
organs :: [ItemKind]
organs =
    [ tire
    , exhaustFan
    , fist
    , foot
    , armoredSkin
    , camera
    , eye6
    , ear6
    , sapientBrain
    , carComputer
    , cpu
    , bonusHP
    , braced
    , asleep
    , impressed ]

-- * No-cooldown melee damage organs without effects

tire :: ItemKind
tire = ItemKind
  { isymbol  = toContentSymbol '@'
  , iname    = "tire"
  , ifreq    = [(S_TIRE, 1)]
  , iflavour = zipPlain [Black]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "drive"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "Summer rubber tire"
  , ikit     = []
  }
exhaustFan :: ItemKind
exhaustFan = ItemKind
  { isymbol  = toContentSymbol '*'
  , iname    = "exhaust fan"
  , ifreq    = [(S_EXHAUST_FAN, 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 3
  , irarity  = [(1, 1)]
  , iverbHit = "swirl"
  , iweight  = 50
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "A cooling exhaust fan, watch out for your fingers!"
  , ikit     = []
  }
fist :: ItemKind
fist = ItemKind
  { isymbol  = toContentSymbol ','
  , iname    = "fist"
  , ifreq    = [(S_FIST, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot :: ItemKind
foot = fist
  { iname    = "foot"
  , ifreq    = [(S_FOOT, 1)]
  , iverbHit = "kick"
  , idamage  = 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
                 -- great example of tutorial hints inside a flavourful text
  }

armoredSkin :: ItemKind
armoredSkin = ItemKind
  { isymbol  = toContentSymbol ','
  , iname    = "armored skin"
  , ifreq    = [(S_ARMORED_SKIN, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Homemade armour is just as good."  -- hmm, it may get confused with leather armor jackets, etc.
  , ikit     = []
  }

-- * Sense organs

eye :: Int -> GroupName ItemKind -> ItemKind
eye n grp = armoredSkin
  { iname    = "eye"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = "A piercing stare."
  }
camera = armoredSkin
  { iname    = "camera"
  , ifreq    = [(S_CAMERA, 1)]
  , icount   = 1
  , iverbHit = "record"
  , iaspects = [ AddSkill SkSight (intToDice 6)
               , SetFlag Durable ]
  , idesc    = "An RGB camera"
  }
eye6 = eye 6 S_EYE_6
ear :: Int -> GroupName ItemKind -> ItemKind
ear n grp = armoredSkin
  { iname    = "ear"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "overhear"
  , iaspects = [ AddSkill SkHearing (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
ear6 = ear 6 S_EAR_6

-- * Assorted

sapientBrain :: ItemKind
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [(S_SAPIENT_BRAIN, 1)]
  , iverbHit = "outbrain"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 4]  -- can use all stairs; dig rubble, ice
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               ++ [AddSkill SkApply 1]  -- can use most items, not just foods
               ++ [SetFlag Durable]
  , idesc    = ""
  }
carComputer :: ItemKind
carComputer = armoredSkin
  { iname    = "car computer"
  , ifreq    = [(S_CAR_COMPUTER, 1)]
  , iverbHit = "blank"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]
               ++ [AddSkill SkAlter 1]
               ++ [AddSkill SkWait 2]
               ++ [AddSkill SkDisplace (-1)]
               ++ [AddSkill SkMoveItem (-1)]
               ++ [AddSkill SkProject (-1)]
               ++ [SetFlag Durable]
  , idesc    = "A primitive computer used in cars"
  }
cpu :: ItemKind
cpu = armoredSkin
  { iname    = "cpu"
  , ifreq    = [(S_CPU, 1)]
  , iverbHit = "blank"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]
               ++ [AddSkill SkAlter 1]
               ++ [AddSkill SkDisplace (-1)]
               ++ [AddSkill SkMoveItem (-1)]
               ++ [AddSkill SkProject (-1)]
               ++ [SetFlag Durable]
  , idesc    = "The central processing unit"
  }

-- * Special

bonusHP :: ItemKind
bonusHP = armoredSkin
  { isymbol  = toContentSymbol 'H'  -- '+' reserved for conditions
  , iname    = "extra HP"
  , ifreq    = [(S_BONUS_HP, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddSkill SkMaxHP 1]
  , idesc    = "Growing up in a privileged background gave you the training and the discrete garment accessories that improve your posture and resilience."
  }
braced :: ItemKind
braced = armoredSkin
  { isymbol  = toContentSymbol 'B'
  , iname    = "braced"
  , ifreq    = [(S_BRACED, 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "brace"
  , iweight  = 0
  , iaspects = [ AddSkill SkArmorMelee 50, AddSkill SkArmorRanged 25
               , AddSkill SkHearing 10
               , SetFlag Condition ] -- hack: display as condition
  , idesc    = "Apart of increased resilience to attacks, being braced protects from displacement by foes and other forms of forced translocation, e.g., pushing or pulling."
  }
asleep :: ItemKind
asleep = armoredSkin
  { isymbol  = toContentSymbol 'S'
  , iname    = "asleep"
  , ifreq    = [(S_ASLEEP, 1)]
  , iflavour = zipPlain [BrGreen]  -- regenerates HP (very slowly)
  , icount   = 5
  , iverbHit = "slay"
  , iweight  = 0
  , iaspects = [AddSkill sk (-1) | sk <- [SkMove .. SkApply]]
               ++ [ AddSkill SkMelee 1, AddSkill SkAlter 1, AddSkill SkWait 1
                  , AddSkill SkSight (-3), AddSkill SkArmorMelee (-10)
                  , SetFlag Condition ]  -- hack: display as condition
  , idesc    = "Sleep helps to regain health, albeit extremely slowly. Being asleep makes you vulnerable, with gradually diminishing effects as the slumber wears off over several turns. Any non-idle action, not only combat but even yawning or stretching removes a sizable portion of the sleepiness."
  }
impressed :: ItemKind
impressed = armoredSkin
  { isymbol  = toContentSymbol 'I'
  , iname    = "impressed"  -- keep the same as in @ifreq@, to simplify code
  , ifreq    = [(S_IMPRESSED, 1), (CONDITION, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)  -- to help player notice on HUD
                                          -- and to count as bad condition
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all copies gone
               , SetFlag Condition ]  -- this is really a condition,
                                      -- just not a timed condition
  , ieffects = [ OnSmash $ verbMsgLess "impressed"
               , OnSmash $ verbMsgNoLonger "impressed" ]
                   -- not periodic, so no wear each turn, so only @OnSmash@
  , idesc    = "Being impressed by one's adversary sounds like fun, but on battlefield it equals treason. Almost. Throw in depleted battle calm and it leads to mindless desertion outright."
  }
