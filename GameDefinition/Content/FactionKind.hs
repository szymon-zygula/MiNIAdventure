-- | Definitions of kinds of factions present in a game, both human
-- and computer-controlled.
module Content.FactionKind
  ( pattern CAR_REPRESENTATIVE
  , pattern COMPUTER_REPRESENTATIVE
  , pattern EXPLORER_REPRESENTATIVE
  , pattern EXPLORER_SHORT
  , pattern REPRESENTATIVE
  , groupNamesSingleton, groupNames
  , content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Content.FactionKind
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

import Content.ItemKindActor

-- * Group name patterns

-- |List of faction patterns
groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton =
    [ CAR_REPRESENTATIVE
    , COMPUTER_REPRESENTATIVE
    , EXPLORER_REPRESENTATIVE
    , EXPLORER_SHORT ]

-- |List containing just `REPRESENTATIVE`
groupNames :: [GroupName FactionKind]
groupNames = [REPRESENTATIVE]

-- |Generic pattern for faction representatives
pattern REPRESENTATIVE :: GroupName FactionKind
pattern REPRESENTATIVE = GroupName "representative"
-- |Representative of car faction
pattern CAR_REPRESENTATIVE :: GroupName c
pattern CAR_REPRESENTATIVE = GroupName "car"
-- |Representative of computer faction
pattern COMPUTER_REPRESENTATIVE :: GroupName c
pattern COMPUTER_REPRESENTATIVE = GroupName "computer"
-- |Representative of explorer (player) faction
pattern EXPLORER_REPRESENTATIVE :: GroupName c
pattern EXPLORER_REPRESENTATIVE = GroupName "explorer"
-- |Group name of default explorer kind
pattern EXPLORER_SHORT :: GroupName c
pattern EXPLORER_SHORT = GroupName "explorer short"

-- * Teams

teamCars :: TeamContinuity
teamCars = TeamContinuity 8
teamComputers :: TeamContinuity
teamComputers = TeamContinuity 9

-- * Content

-- |List of all factions
content :: [FactionKind]
content =
    [ factCar
    , factComputer
    , factExplorer
    , factExplorerShort ]


-- * Content

factCar :: FactionKind
factCar = FactionKind
  { fname = "Traffic of cars"
  , ffreq = [(CAR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCars
  , fgroups = [ (CAR, 100) ]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TExplore
  , fspawnsFast = True
  , fhasPointman = True
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = [teamExplorer]
  , falliedTeams = []
  }

factComputer :: FactionKind
factComputer = FactionKind
  { fname = "Batch of computers"
  , ffreq = [(COMPUTER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamComputers
  , fgroups = [ (COMPUTER, 75), (SERVER, 25) ]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , finitDoctrine = TExplore
  , fspawnsFast = True
  , fhasPointman = True
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = [teamExplorer]
  , falliedTeams = []
  }

factExplorer :: FactionKind
factExplorer = FactionKind
  { fname = "Explorer"
  , ffreq = [(EXPLORER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamExplorer
  , fgroups = [(HERO, 100)]  -- don't spam the escapists, etc., in description
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHeroLong
  , fhasGender = True
  , finitDoctrine = TExplore
  , fspawnsFast = False
  , fhasPointman = True
  , fhasUI = True
  , finitUnderAI = False
  , fenemyTeams = []
  , falliedTeams = []
  }
factExplorerShort :: FactionKind
factExplorerShort = factExplorer
  { ffreq = [(EXPLORER_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = []
  }
