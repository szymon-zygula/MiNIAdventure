-- | Definitions of game mode kinds.
module Content.ModeKind
  ( -- * Group name patterns
    groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
  , pattern MINI, pattern RAID, pattern BRAWL, pattern LONG, pattern CRAWL
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
       [MINI, RAID, BRAWL, LONG, CRAWL]

pattern MINI, RAID, BRAWL, LONG, CRAWL :: GroupName ModeKind

groupNames :: [GroupName ModeKind]
groupNames = []

pattern MINI = GroupName "mini"
pattern RAID = GroupName "raid"
pattern BRAWL = GroupName "brawl"
pattern LONG = GroupName "long crawl"
pattern CRAWL = GroupName "crawl"

-- * Content

content :: [ModeKind]
content =
  [mini, raid, brawl, crawl, screensaverRaid, screensaverBrawl, screensaverCrawl]

mini, raid, brawl, crawl, screensaverRaid, screensaverBrawl, screensaverCrawl :: ModeKind

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

raid = ModeKind
  { mname   = "raid (tutorial, 1)"
  , mfreq   = [(RAID, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "This expedition has gone wrong. However, scientific mind does not despair, but analyzes and corrects. Did you perchance awake one animal too many? Did you remember to try using all consumables at your disposal for your immediate survival? Did you choose a challenge with difficulty level within your means? Answer honestly, ponder wisely, experiment methodically.")
              , (Defeated, "Regrettably, the other team snatched the grant, while you were busy contemplating natural phenomena. Science is a competitive sport, as sad as it sounds. It's not enough to make a discovery, you have to get there first.")
              , (Escape, "You've got hold of the machine! Think of the hours of fun taking it apart and putting it back together again! That's a great first step on your quest to solve the typing problems of the world.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Two heroes vs. Spawned enemies"
      , "* Gather gold"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the exit of this maze. Be the first to find it and fund a research team that makes typing accurate and dependable forever."
  , mreason = "In addition to initiating the (loose) game plot, this adventure provides an introductory tutorial. Relax, explore, gather loot, find the way out and escape. With some luck, you won't even need to fight anything."
  , mhint   = "You can't use gathered items in your next encounters, so trigger any consumables at will. Feel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher your score in this encounter."
  }

mini = ModeKind
  { mname = "mini"
  , mfreq   = [(MINI, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterMini
  , mcaves  = cavesMini
  , mendMsg = [ (Killed, "You have died in the depths of the parking lot.")
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

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { mname   = "brawl (tutorial, 2)"
  , mfreq   = [(BRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "The inquisitive scholars turned out to be envious of our deep insight to the point of outright violence. It would still not result in such a defeat and recanting of our thesis if we figured out to use terrain to protect us from missiles or even completely hide our presence. It would also help if we honourably kept our ground together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, research team.")
              , (Conquer, "That's settled: local compactness *is* necessary for relative completeness, given the assumptions.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Your research team disagrees over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that."
      -- Not enough space with square fonts and also this is more of a hint than a flavour: Remember to keep your party together when opponents are spotted, or they might be tempted to silence solitary disputants one by one and so win the altercation.
  , mreason = "In addition to advancing game plot, this encounter trains melee, squad formation and stealth. The battle is symmetric in goals (incapacitate all enemies) and in squad capabilities (only the pointman moves, others either melee or wait)."
  , mhint   =  T.intercalate "\n"
    [ "Run a short distance with Shift or LMB, switch the pointman with Tab, repeat. In open terrain, if you keep distance between teammates, this resembles the leap frog infantry tactics. For best effects, end each sprint behind a cover or concealment."
    , "Observe and mimic the enemies. If you can't see an enemy that apparently can see you, in reversed circumstances you would have the same advantage. Savour the relative fairness --- you won't find any in the main crawl adventure that follows."
    , "If you get beaten repeatedly, try using all consumables you find. Ponder the hints from the defeat message, in particular the one about keeping your party together once the opponents are spotted. However, if you want to discover a winning tactics on your own, make sure to ignore any such tips until you succeed."
    ]
  }

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

screensaverRaid = raid
  { mname   = "auto-raid (1)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = []
  , mattract = True
  }

screensaverCrawl = crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = []
  , mattract = True
  }

rosterMini, rosterRaid, rosterBrawl, rosterCrawl :: Roster

rosterMini = 
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , [(-2, 2, ANIMAL)] )
  , ( EXPLORER_SHORT
    , [(-2, 2, HERO)] )
  , ( COMPETITOR_SHORT
    , [(-2, 1, HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterRaid =
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , [(-2, 2, ANIMAL)] )
  , ( EXPLORER_SHORT
    , [(-2, 2, HERO)] )
  , ( COMPETITOR_SHORT
    , [(-2, 1, HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterBrawl =
  [ ( EXPLORER_NO_ESCAPE
    , [(-2, 3, BRAWLER_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(-2, 3, BRAWLER_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterCrawl =
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , -- Fun from the start to avoid empty initial level:
      [ (-1, 1 + 1 `d` 2, ANIMAL)
      -- Huge battle at the end:
      , (-10, 100, MOBILE_ANIMAL) ] )
  , ( EXPLORER_REPRESENTATIVE
        -- start on stairs so that stash is handy
    , [(-1, 3, HERO)] )
  , ( MONSTER_REPRESENTATIVE
    , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] ) ]

cavesMini, cavesRaid, cavesBrawl, cavesCrawl :: Caves

cavesMini = [([-2], [CAVE_MINI])] -- Tablica to numery poziomow

cavesRaid = [([-2], [CAVE_RAID])]

cavesBrawl = [([-2], [CAVE_BRAWL])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([-1], [CAVE_OUTERMOST])
  , ([-2], [CAVE_SHALLOW_ROGUE])
  , ([-3], [CAVE_EMPTY])
  , ([-4, -5, -6], [DEFAULT_RANDOM, CAVE_ROGUE, CAVE_ARENA])
  , ([-7, -8], [CAVE_ROGUE, CAVE_SMOKING])
  , ([-9], [CAVE_LABORATORY])
  , ([-10], [CAVE_MINE]) ]

cavesCrawl = listCrawl
