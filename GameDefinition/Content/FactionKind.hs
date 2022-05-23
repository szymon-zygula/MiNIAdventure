-- | Definitions of kinds of factions present in a game, both human
-- and computer-controlled.
module Content.FactionKind
  ( pattern CAR_REPRESENTATIVE
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
  , pattern CONVICT_REPRESENTATIVE
  , pattern MONSTER_REPRESENTATIVE
  , pattern MONSTER_ANTI
  , pattern MONSTER_ANTI_CAPTIVE
  , pattern MONSTER_ANTI_PACIFIST
  , pattern MONSTER_CAPTIVE
  , pattern MONSTER_CAPTIVE_NARRATING
  , pattern HORROR_REPRESENTATIVE
  , pattern HORROR_CAPTIVE
  , pattern HORROR_PACIFIST
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
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

import Content.ItemKindActor
import Content.ItemKindOrgan

-- * Group name patterns

groupNamesSingleton :: [GroupName FactionKind]
groupNamesSingleton =
    [ CAR_REPRESENTATIVE
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
    , CIVILIAN_REPRESENTATIVE
    , CONVICT_REPRESENTATIVE
    , MONSTER_REPRESENTATIVE
    , MONSTER_ANTI
    , MONSTER_ANTI_CAPTIVE
    , MONSTER_ANTI_PACIFIST
    , MONSTER_CAPTIVE
    , MONSTER_CAPTIVE_NARRATING
    , HORROR_REPRESENTATIVE
    , HORROR_CAPTIVE
    , HORROR_PACIFIST]

groupNames :: [GroupName FactionKind]
groupNames = [REPRESENTATIVE]

pattern REPRESENTATIVE :: GroupName FactionKind
pattern REPRESENTATIVE = GroupName "representative"
pattern CAR_REPRESENTATIVE :: GroupName c
pattern CAR_REPRESENTATIVE = GroupName "car"
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
pattern CONVICT_REPRESENTATIVE :: GroupName c
pattern CONVICT_REPRESENTATIVE = GroupName "convict"
pattern MONSTER_REPRESENTATIVE :: GroupName c
pattern MONSTER_REPRESENTATIVE = GroupName "monster"
pattern MONSTER_ANTI :: GroupName c
pattern MONSTER_ANTI = GroupName "monster anti"
pattern MONSTER_ANTI_CAPTIVE :: GroupName c
pattern MONSTER_ANTI_CAPTIVE = GroupName "monster anti captive"
pattern MONSTER_ANTI_PACIFIST :: GroupName c
pattern MONSTER_ANTI_PACIFIST = GroupName "monster anti pacifist"
pattern MONSTER_CAPTIVE :: GroupName c
pattern MONSTER_CAPTIVE = GroupName "monster captive"
pattern MONSTER_CAPTIVE_NARRATING :: GroupName c
pattern MONSTER_CAPTIVE_NARRATING = GroupName "monster captive narrating"
pattern HORROR_REPRESENTATIVE :: GroupName c
pattern HORROR_REPRESENTATIVE = GroupName "horror"
pattern HORROR_CAPTIVE :: GroupName c
pattern HORROR_CAPTIVE = GroupName "horror captive"
pattern HORROR_PACIFIST :: GroupName c
pattern HORROR_PACIFIST = GroupName "horror pacifist"

-- * Teams

teamCompetitor :: TeamContinuity
teamCompetitor = TeamContinuity 2
teamCivilian :: TeamContinuity
teamCivilian = TeamContinuity 3
teamConvict :: TeamContinuity
teamConvict = TeamContinuity 4
teamMonster :: TeamContinuity
teamMonster = TeamContinuity 5
teamHorror :: TeamContinuity
teamHorror = TeamContinuity 7
teamCars :: TeamContinuity
teamCars = TeamContinuity 8
teamOther :: TeamContinuity
teamOther = TeamContinuity 10

-- * Content

content :: [FactionKind]
content =
    [ factCar
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
    , factCivilian
    , factConvict
    , factMonster
    , factMonsterAnti
    , factMonsterAntiCaptive
    , factMonsterAntiPacifist
    , factMonsterCaptive
    , factMonsterCaptiveNarrating
    , factHorror
    , factHorrorCaptive
    , factHorrorPacifist ]


-- * Content

-- ** teamExplorer

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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
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
  , fenemyTeams = [teamCompetitor, teamMonster, teamHorror]
  , falliedTeams = []
  }
factExplorerShort :: FactionKind
factExplorerShort = factExplorer
  { ffreq = [(EXPLORER_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = [teamMonster, teamHorror]
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
  , fenemyTeams = [teamExplorer, teamMonster, teamHorror]
  , falliedTeams = []
  }
factCompetitorShort :: FactionKind
factCompetitorShort = factCompetitor
  { fname = "Indigo Founder"  -- early
  , ffreq = [(COMPETITOR_SHORT, 1)]
  , fhiCondPoly = hiHeroShort
  , fenemyTeams = [teamMonster, teamHorror]
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
  , fenemyTeams = [teamMonster, teamHorror]
  , falliedTeams = []
  }

-- ** teamConvict, different demographics

factConvict :: FactionKind
factConvict = factCivilian
  { fname = "Hunam Convict"
  , ffreq = [(CONVICT_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamConvict
  , fhasPointman = True  -- convicts organize better
  , finitUnderAI = True
  , fenemyTeams = [teamMonster, teamHorror]
  , falliedTeams = []
  }

-- ** teamMonster

factMonster :: FactionKind
factMonster = FactionKind
  { fname = "Monster Hive"
  , ffreq = [(MONSTER_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamMonster
  , fgroups = [ (MONSTER, 100)
              , (MOBILE_MONSTER, 1) ]
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
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
-- This has continuity @teamMonster@, despite being playable.
factMonsterAnti :: FactionKind
factMonsterAnti = factMonster
  { ffreq = [(MONSTER_ANTI, 1)]
  , fhasUI = True
  , finitUnderAI = False
  }
factMonsterAntiCaptive :: FactionKind
factMonsterAntiCaptive = factMonsterAnti
  { ffreq = [(MONSTER_ANTI_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factMonsterAntiPacifist :: FactionKind
factMonsterAntiPacifist = factMonsterAntiCaptive
  { ffreq = [(MONSTER_ANTI_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }
factMonsterCaptive :: FactionKind
factMonsterCaptive = factMonster
  { ffreq = [(MONSTER_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factMonsterCaptiveNarrating :: FactionKind
factMonsterCaptiveNarrating = factMonsterAntiCaptive
  { ffreq = [(MONSTER_CAPTIVE_NARRATING, 1)]
  , fhasUI = True
  }

-- ** teamHorror, not much of a continuity intended, but can't be ignored

-- | A special faction, for summoned actors that don't belong to any
-- of the main factions of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror faction should be added to host them.
factHorror :: FactionKind
factHorror = FactionKind
  { fname = "Horror Den"
  , ffreq = [(HORROR_REPRESENTATIVE, 1), (REPRESENTATIVE, 1)]
  , fteam = teamHorror
  , fgroups = [(IK.HORROR, 100)]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasGender = False
  , finitDoctrine = TPatrol  -- disoriented
  , fspawnsFast = False
  , fhasPointman = False
  , fhasUI = False
  , finitUnderAI = True
  , fenemyTeams = [teamExplorer, teamCompetitor, teamCivilian, teamConvict]
  , falliedTeams = []
  }
factHorrorCaptive :: FactionKind
factHorrorCaptive = factHorror
  { ffreq = [(HORROR_CAPTIVE, 1)]
  , fneverEmpty = True
  }
factHorrorPacifist :: FactionKind
factHorrorPacifist = factHorrorCaptive
  { ffreq = [(HORROR_PACIFIST, 1)]
  , fenemyTeams = []
  , falliedTeams = []
  }
