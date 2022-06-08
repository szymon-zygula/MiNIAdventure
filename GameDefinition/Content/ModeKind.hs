-- | Definitions of game mode kinds.
module Content.ModeKind
  ( groupNamesSingleton
  , groupNames
  , content
#ifdef EXPOSE_INTERNAL
  , pattern MINI
  , pattern LONG
  , pattern CRAWL
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Content.CaveKind (CaveKind, pattern DEFAULT_RANDOM)
import Game.LambdaHack.Content.FactionKind (Outcome (..))
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.FactionKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor

-- * Group name patterns

groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [MINI, LONG, CRAWL]

groupNames :: [GroupName ModeKind]
groupNames = []

pattern MINI :: GroupName c
pattern MINI = GroupName "mini"
pattern LONG :: GroupName c
pattern LONG = GroupName "long crawl"
pattern CRAWL :: GroupName c
pattern CRAWL = GroupName "crawl"

-- * Content

content :: [ModeKind]
content =
  [ mini
  , crawl
  , screensaverMini
  ]

-- What other symmetric (two only-one-moves factions) and asymmetric vs crowd
-- scenarios make sense (e.g., are good for a tutorial or for standalone
-- extreme fun or are impossible as part of a crawl)?
-- sparse melee at night: no, shade ambush in brawl is enough
-- dense melee: no, keeping big party together is a chore and big enemy
--   party is less fun than huge enemy party
-- crowd melee in daylight: no, possible in crawl and at night is more fun
-- sparse ranged at night: no, less fun than dense and if no reaction fire,
--   just a camp fest or firing blindly
-- dense ranged in daylight: no, less fun than at night with flares
-- crowd ranged: no, fish in a barrel, less predictable and more fun inside
--   crawl, even without reaction fire

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

crawl :: ModeKind
crawl = ModeKind
  { mname   = "long crawl (main)"
  , mfreq   = [(LONG, 1), (CRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, killedMsg)
              , (Escape, "It's better to live to tell the tale than to choke on more than one can swallow. There was no more exquisite cultural artifacts and glorious scientific wonders in these forbidding tunnels anyway. Or were there?") ]
  , mrules  = T.intercalate "\n"
      [ "* Many levels"
      , "* Three heroes vs. Spawned enemies"
      , "* Gather gold, gems and elixirs"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "Enjoy the peaceful seclusion of these cold austere tunnels, but don't let wanton curiosity, greed and the ever-creeping abstraction madness keep you down there for too long. If you find survivors (whole or perturbed or segmented) of the past scientific missions, exercise extreme caution and engage or ignore at your discretion."
  , mreason = "This is the main, longest and most replayable scenario of the game. It's crucial that you gather the most interesting cultural artifacts such as gold, gems and elixirs. Equally importantly, you have to limit the permanent sanity deterioration of your scientific expedition members by minimizing the time they are exposed to the horrors of the underworld."
  , mhint   = "If you keep dying, attempt the subsequent adventures as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition. Also experimenting with the initial adventures may answer some questions you didn't have when you attempted them originally."
  }
 where
   killedMsg = T.intercalate "\n"
     [ "To think that followers of science and agents of enlightenment would earn death as their reward! Where did we err in our ways? Perhaps nature should not have been disturbed so brashly and the fell beasts woken up from their slumber so eagerly?"
     , "Perhaps the gathered items should have been used for scientific experiments on the spot rather than hoarded as if of base covetousness? Or perhaps the challenge, chosen freely but without the foreknowledge of the grisly difficulty, was insurmountable and forlorn from the start, despite the enormous power of educated reason at out disposal?"
     ]

screensaverMini :: ModeKind
screensaverMini = mini
  { mname   = "auto-mini (1)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

rosterMini :: [(GroupName c1, [(Int, Dice, GroupName c2)])]
rosterMini =
  [ ( COMPUTER_REPRESENTATIVE
    , [(-2, 2, COMPUTER)] )
  , ( EXPLORER_SHORT
    , [(-2, 2, HERO)] ) ]

rosterCrawl :: [(GroupName c1, [(Int, Dice, GroupName c2)])]
rosterCrawl =
  [ ( EXPLORER_REPRESENTATIVE
    , [(-1, 3, HERO)] )
  ]

cavesMini :: [([Int], [GroupName c])]
cavesMini = [([-2], [CAVE_MINI_LABORATORY])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([-1], [CAVE_OUTERMOST])
  , ([-2], [CAVE_SHALLOW_ROGUE])
  , ([-3], [CAVE_EMPTY])
  , ([-4, -5, -6], [DEFAULT_RANDOM, CAVE_ROGUE, CAVE_ARENA])
  , ([-7, -8], [CAVE_ROGUE, CAVE_SMOKING])
  , ([-9], [CAVE_LABORATORY])
  , ([-10], [CAVE_MINE]) ]

cavesCrawl :: [([Int], [GroupName CaveKind])]
cavesCrawl = listCrawl
