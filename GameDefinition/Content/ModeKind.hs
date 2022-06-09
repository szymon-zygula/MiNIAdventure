-- | Definitions of game mode kinds.
module Content.ModeKind
  ( groupNamesSingleton
  , groupNames
  , content
  , pattern MINI
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Content.FactionKind (Outcome (..))
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.FactionKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor

-- |Group name patterns
groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [MINI]

-- |Mode kind group names. It is empty.
groupNames :: [GroupName ModeKind]
groupNames = []

-- |Pattern of the main game mode
pattern MINI :: GroupName c
pattern MINI = GroupName "mini"

-- |List of all game modes (so just the main mode and screensaver)
content :: [ModeKind]
content =
  [ mini
  , screensaverMini
  ]

mini :: ModeKind
mini = ModeKind
  { mname = "mini"
  , mfreq   = [(MINI, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterMini
  , mcaves  = cavesMini
  , mendMsg = [ (Killed, "You have died in the depths of the faculty.")
              , (Defeated, "Regrettably, it looks like you have not acquired the necessary amount of ECTS points.")
              , (Escape, "You've got hold of the ELMiTA book!") ]
  , mrules  = T.intercalate "\n"
      [ "* Do not fail"
      , "* Find the ELMiTA book"
      ]
    , mdesc = "The original copy of the book \"Elementy Lingwistyki Matematycznej i Teorii Automatów\" is hidden in MiNI underground parking lot. You have to find it."
    , mreason = "Haskell functional programming project"
    , mhint = "There are no hints, be careful."
  }

screensaverMini :: ModeKind
screensaverMini = mini
  { mname   = "auto-mini (1)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

rosterMini :: [(GroupName c1, [(Int, Dice, GroupName c2)])]
rosterMini =
  [ ( COMPUTER_REPRESENTATIVE, [(-1, 4, COMPUTER), (-2, 4, SERVER)] )
  , ( CAR_REPRESENTATIVE, [(-3, 4, CAR)] )
  , ( EXPLORER_SHORT, [(-1, 4, HERO)] ) ]

cavesMini :: [([Int], [GroupName c])]
cavesMini =
  [ ([-1], [CAVE_MINI_LABORATORY])
  , ([-2], [CAVE_SERVER_ROOM])
  , ([-3], [CAVE_PARKING_LOT]) ]
