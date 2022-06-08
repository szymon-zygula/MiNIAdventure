-- | Definitions of kinds of factions present in a game, both human
-- and computer-controlled.
module Content.FactionKind
  ( pattern CAR_REPRESENTATIVE
  , pattern COMPUTER_REPRESENTATIVE
  , pattern EXPLORER_REPRESENTATIVE
  , pattern EXPLORER_SHORT
  , pattern EXPLORER_MEDIUM
  , pattern EXPLORER_TRAPPED
  , pattern EXPLORER_AUTOMATED
  , pattern EXPLORER_AUTOMATED_TRAPPED
  , pattern EXPLORER_CAPTIVE
  , pattern EXPLORER_PACIFIST
  , pattern COMPETITOR_REPRESENTATIVE
  , pattern COMPETITOR_SHORT
  , pattern CIVILIAN_REPRESENTATIVE
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

groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton =
    [ CAR_REPRESENTATIVE
    , COMPUTER_REPRESENTATIVE
    , EXPLORER_REPRESENTATIVE
    , EXPLORER_SHORT
    , EXPLORER_MEDIUM
    , EXPLORER_TRAPPED
    , EXPLORER_AUTOMATED
    , EXPLORER_AUTOMATED_TRAPPED
    , EXPLORER_CAPTIVE
    , EXPLORER_PACIFIST
    , COMPETITOR_REPRESENTATIVE
    , COMPETITOR_SHORT
    , CIVILIAN_REPRESENTATIVE ]

groupNames :: [GroupName FactionKind]
groupNames = [REPRESENTATIVE]

pattern REPRESENTATIVE :: GroupName FactionKind
pattern REPRESENTATIVE = GroupName "representative"
pattern CAR_REPRESENTATIVE :: GroupName c
pattern CAR_REPRESENTATIVE = GroupName "car"
pattern COMPUTER_REPRESENTATIVE :: GroupName c
pattern COMPUTER_REPRESENTATIVE = GroupName "computer"
pattern EXPLORER_REPRESENTATIVE :: GroupName c
pattern EXPLORER_REPRESENTATIVE = GroupName "explorer"
pattern EXPLORER_SHORT :: GroupName c
pattern EXPLORER_SHORT = GroupName "explorer short"
pattern EXPLORER_MEDIUM :: GroupName c
pattern EXPLORER_MEDIUM = GroupName "explorer medium"
pattern EXPLORER_TRAPPED :: GroupName c
pattern EXPLORER_TRAPPED = GroupName "explorer trapped"
pattern EXPLORER_AUTOMATED :: GroupName c
pattern EXPLORER_AUTOMATED = GroupName "explorer automated"
pattern EXPLORER_AUTOMATED_TRAPPED :: GroupName c
pattern EXPLORER_AUTOMATED_TRAPPED = GroupName "explorer automated trapped"
pattern EXPLORER_CAPTIVE :: GroupName c
pattern EXPLORER_CAPTIVE = GroupName "explorer captive"
pattern EXPLORER_PACIFIST :: GroupName c
pattern EXPLORER_PACIFIST = GroupName "explorer pacifist"
pattern COMPETITOR_REPRESENTATIVE :: GroupName c
pattern COMPETITOR_REPRESENTATIVE = GroupName "competitor"
pattern COMPETITOR_SHORT :: GroupName c
pattern COMPETITOR_SHORT = GroupName "competitor short"
pattern CIVILIAN_REPRESENTATIVE :: GroupName c
pattern CIVILIAN_REPRESENTATIVE = GroupName "civilian"

-- * Teams

teamCompetitor :: TeamContinuity
teamCompetitor = TeamContinuity 2
teamCivilian :: TeamContinuity
teamCivilian = TeamContinuity 3
teamCars :: TeamContinuity
teamCars = TeamContinuity 8
teamComputers :: TeamContinuity
teamComputers = TeamContinuity 9

-- * Content

content :: [FactionKind]
content =
    [ factCar
    , factComputer
    , factExplorer
    , factExplorerShort
    , factExplorerMedium
    , factExplorerTrapped
    , factExplorerAutomated
    , factExplorerAutomatedTrapped
    , factExplorerCaptive
    , factExplorerPacifist
    , factCompetitor
    , factCompetitorShort
    , factCivilian ]


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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian]
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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian]
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
  , fenemyTeams = [teamCompetitor]
  , falliedTeams = []
  }
factExplorerShort :: FactionKind
factExplorerShort = factExplorer
  { ffreq = [(EXPLORER_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = []
  }
factExplorerMedium :: FactionKind
factExplorerMedium = factExplorer
  { ffreq = [(EXPLORER_MEDIUM, 1)]
  , fhiCondPoly = hiHeroMedium
  }
factExplorerTrapped :: FactionKind
factExplorerTrapped = factExplorer
  { ffreq = [(EXPLORER_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }
factExplorerAutomated :: FactionKind
factExplorerAutomated = factExplorer
  { ffreq = [(EXPLORER_AUTOMATED, 1)]
  , fhasUI = False
  , finitUnderAI = True
  }
factExplorerAutomatedTrapped :: FactionKind
factExplorerAutomatedTrapped = factExplorerAutomated
  { ffreq = [(EXPLORER_AUTOMATED_TRAPPED, 1)]
  , fcanEscape = False
  , fhiCondPoly = hiHeroLong
  }
factExplorerCaptive :: FactionKind
factExplorerCaptive = factExplorer
  { ffreq = [(EXPLORER_CAPTIVE, 1)]
  , fneverEmpty = True  -- already there
  }
factExplorerPacifist :: FactionKind
factExplorerPacifist = factExplorerCaptive
  { ffreq = [(EXPLORER_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }

-- ** teamCompetitor, symmetric opponents of teamExplorer

factCompetitor :: FactionKind
factCompetitor = factExplorer
  { fname = "Indigo Researcher"
  , ffreq = [(COMPETITOR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCompetitor
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = [teamExplorer]
  , falliedTeams = []
  }
factCompetitorShort :: FactionKind
factCompetitorShort = factCompetitor
  { fname = "Indigo Founder"  -- early
  , ffreq = [(COMPETITOR_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = []
  }

-- ** teamCivilian

factCivilian :: FactionKind
factCivilian = FactionKind
  { fname = "Civilian"
  , ffreq = [(CIVILIAN_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamCivilian
  , fgroups = [(HERO, 100), (CIVILIAN, 100)]  -- symmetric vs player
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = hiHeroMedium
  , fhasGender = True
  , finitDoctrine = TPatrol
  , fspawnsFast = False
  , fhasPointman = False  -- unorganized
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = []
  , falliedTeams = []
  }
