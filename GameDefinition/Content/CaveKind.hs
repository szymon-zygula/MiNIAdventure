-- | Definitions of of cave kinds. Every level in the game is an instantiated
-- cave kind.
module Content.CaveKind
  ( -- * Group name patterns
    pattern CAVE_MINI, pattern CAVE_ROGUE, pattern CAVE_ARENA, pattern CAVE_SMOKING, pattern CAVE_LABORATORY, pattern CAVE_NOISE, pattern CAVE_MINE, pattern CAVE_EMPTY, pattern CAVE_SHALLOW_ROGUE, pattern CAVE_OUTERMOST, pattern CAVE_RAID, pattern CAVE_BRAWL
  , groupNamesSingleton, groupNames
  , -- * Content
    content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Ratio

import           Game.LambdaHack.Content.CaveKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Core.Dice
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

import Content.ItemKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor
import Content.PlaceKind hiding (content, groupNames, groupNamesSingleton)
import Content.TileKind hiding (content, groupNames, groupNamesSingleton)

-- * Group name patterns

groupNamesSingleton :: [GroupName CaveKind]
groupNamesSingleton = []

groupNames :: [GroupName CaveKind]
groupNames =
       [CAVE_MINI, CAVE_ROGUE, CAVE_ARENA, CAVE_SMOKING, CAVE_LABORATORY, CAVE_NOISE, CAVE_MINE, CAVE_EMPTY, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL]

pattern CAVE_MINI, CAVE_ROGUE, CAVE_ARENA, CAVE_SMOKING, CAVE_LABORATORY, CAVE_NOISE, CAVE_MINE, CAVE_EMPTY, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL :: GroupName CaveKind

pattern CAVE_MINI = GroupName "caveMini"
pattern CAVE_ROGUE = GroupName "caveRogue"
pattern CAVE_ARENA = GroupName "caveArena"
pattern CAVE_SMOKING = GroupName "caveSmoking"
pattern CAVE_LABORATORY = GroupName "caveLaboratory"
pattern CAVE_NOISE = GroupName "caveNoise"
pattern CAVE_MINE = GroupName "caveMine"
pattern CAVE_EMPTY = GroupName "caveEmpty"
pattern CAVE_SHALLOW_ROGUE = GroupName "caveShallowRogue"
pattern CAVE_OUTERMOST = GroupName "caveOutermost"
pattern CAVE_RAID = GroupName "caveRaid"
pattern CAVE_BRAWL = GroupName "caveBrawl"

-- * Content

content :: [CaveKind]
content =
  [mini, rogue, arena, smoking, laboratory, noise, mine, empty, outermost, shallowRogue, raid, brawl]

mini, rogue, arena, smoking, laboratory, noise, mine, empty, outermost, shallowRogue, raid, brawl :: CaveKind

-- * Underground caves; most of mediocre height and size

rogue = CaveKind
  { cname         = "A maze of twisty passages"
  , cfreq         = [(DEFAULT_RANDOM, 100), (CAVE_ROGUE, 1)]
  , cXminSize     = 80
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 10) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 40
  , cdarkOdds     = 1 `d` 54 + 1 `dL` 20
      -- most rooms lit, to compensate for dark corridors
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%6
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 70  -- the maze requires time to explore
  , cactorFreq    = [(MONSTER, 60), (ANIMAL, 40)]
  , citemNum      = 6 `d` 5 + 10 - 10 `dL` 1
      -- deep down quality over quantity; generally quite random,
      -- for interesting replays at the cost of unreliable balance
  , citemFreq     = [(IK.COMMON_ITEM, 40), (IK.TREASURE, 60)]
  , cplaceFreq    = [(ROGUE, 1)]
  , cpassable     = False
  , clabyrinth    = False
  , cdefTile      = FILLER_WALL
  , cdarkCorTile  = FLOOR_CORRIDOR_DARK
  , clitCorTile   = FLOOR_CORRIDOR_LIT
  , cwallTile     = FILLER_WALL
  , ccornerTile   = FILLER_WALL
  , cfenceTileN   = S_BASIC_OUTER_FENCE
  , cfenceTileE   = S_BASIC_OUTER_FENCE
  , cfenceTileS   = S_BASIC_OUTER_FENCE
  , cfenceTileW   = S_BASIC_OUTER_FENCE
  , cfenceApart   = False
  , cminStairDist = 20
  , cmaxStairsNum = 1 + 1 `d` 2
  , cescapeFreq   = []
  , cstairFreq    = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cstairAllowed = []
  , cskip         = []
  , cinitSleep    = InitSleepPermitted
  , cdesc         = "Winding tunnels stretch into the dark."
  }  -- no lit corridors cave alternative, since both lit # and . look bad here
arena = rogue
  { cname         = "Dusty underground library"
  , cfreq         = [(DEFAULT_RANDOM, 60), (CAVE_ARENA, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 3 + 17) (1 `d` 3 + 4)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 6
  , cmaxPlaceSize = DiceXY 16 12
  , cdarkOdds     = 49 + 1 `d` 10  -- almost all rooms dark (1 in 10 lit)
  -- Light is not too deadly, because not many obstructions and so
  -- foes visible from far away and few foes have ranged combat
  -- at shallow depth.
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 1
  , cmaxVoid      = 1%8
  , chidden       = 0
  , cactorCoeff   = 70  -- small open level, don't rush the player
  , cactorFreq    = [(MONSTER, 30), (ANIMAL, 70)]
  , citemNum      = 4 `d` 5  -- few rooms
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.TREASURE, 40)
                    , (IK.ANY_SCROLL, 40) ]
  , cplaceFreq    = [(ARENA, 1)]
  , cpassable     = True
  , cdefTile      = ARENA_SET_LIT
  , cdarkCorTile  = TRAIL_LIT  -- let trails give off light
  , clitCorTile   = TRAIL_LIT  -- may be rolled different than the above
  , cminStairDist = 15
  , cmaxStairsNum = 1 `d` 2
  , cstairFreq    = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepAlways
  , cdesc         = "The shelves groan with dusty books and tattered scrolls. Subtle snoring can be heard from a distance."
  }
smoking = arena
  { cname         = "Smoking rooms"
  , cfreq         = [(CAVE_SMOKING, 1)]
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth.
  , cnightOdds    = 51  -- always night
  , citemNum      = 6 `d` 5  -- rare, so make it exciting
  , citemFreq     = [(IK.COMMON_ITEM, 20), (IK.TREASURE, 40), (IK.ANY_GLASS, 40)]
  , cdefTile      = ARENA_SET_DARK
  , cdesc         = "Velvet couches exude the strong smell of tobacco."
  }
laboratory = rogue
  { cname         = "Burnt laboratory"
  , cfreq         = [(CAVE_LABORATORY, 1)]
  , cXminSize     = 60
  , cYminSize     = 21
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 7 5
  , cmaxPlaceSize = DiceXY 10 40
  , cnightOdds    = 0  -- always day so that the corridor smoke is lit
  , cauxConnects  = 1%5
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 1%2
  , cactorFreq    = [(MONSTER, 30), (ANIMAL, 70)]
  , citemNum      = 6 `d` 5  -- reward difficulty
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.TREASURE, 40)
                    , (IK.EXPLOSIVE, 40) ]
  , cplaceFreq    = [(LABORATORY, 1)]
  , cdarkCorTile  = LAB_TRAIL_LIT  -- let lab smoke give off light always
  , clitCorTile   = LAB_TRAIL_LIT
  , cmaxStairsNum = 2
  , cstairFreq    = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Shattered glassware and the sharp scent of spilt chemicals show that something terrible happened here."
  }
noise = rogue
  { cname         = "Leaky burrowed sediment"
  , cfreq         = [(DEFAULT_RANDOM, 30), (CAVE_NOISE, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 5 + 12) 6
  , cminPlaceSize = DiceXY 8 5
  , cmaxPlaceSize = DiceXY 20 20
  , cdarkOdds     = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightOdds    = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cdoorChance   = 1  -- to avoid lit quasi-door tiles
  , chidden       = 0
  , cactorCoeff   = 100  -- the maze requires time to explore; also, small
  , cactorFreq    = [(MONSTER, 80), (ANIMAL, 20)]
  , citemNum      = 6 `d` 5  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [(NOISE, 1)]
  , clabyrinth    = True
  , cdefTile      = NOISE_SET_LIT
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = DAMP_FLOOR_DARK
  , clitCorTile   = DAMP_FLOOR_LIT
  , cminStairDist = 15
  , cstairFreq    = [ (CLOSED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "Soon, these passages will be swallowed up by the mud."
  }
mine = noise
  { cname         = "Frozen derelict mine"
  , cfreq         = [(CAVE_MINE, 1)]
  , cnightOdds    = 51  -- easier variant, but looks sinister
  , citemNum      = 10 `d` 4  -- an incentive to explore the final labyrinth
  , citemFreq     = [(IK.COMMON_ITEM, 20), (GEM, 20)]
                      -- can't be "valuable" or template items generated
  , cplaceFreq    = [(NOISE, 1), (MINE, 99)]
  , clabyrinth    = True
  , cdefTile      = POWER_SET_DARK
  , cstairFreq    = [ (GATED_CLOSED_STAIRCASE, 50)
                    , (GATED_OPEN_STAIRCASE, 50)
                    , (GATED_TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "Pillars of shining ice create a frozen labyrinth."
  }
empty = rogue
  { cname         = "Tall cavern"
  , cfreq         = [(CAVE_EMPTY, 1)]
  , ccellSize     = DiceXY (2 `d` 2 + 11) (1 `d` 2 + 8)
  , cminPlaceSize = DiceXY 13 11
  , cmaxPlaceSize = DiceXY 37 31  -- favour large rooms
  , cdarkOdds     = 1 `d` 100 + 1 `dL` 100
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 8
  , cactorFreq    = [(ANIMAL, 10), (IMMOBILE_ANIMAL, 90)]
      -- The healing geysers on lvl 3 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. Gyesers on lvl 3 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , citemNum      = 4 `d` 5  -- few rooms and geysers are the boon
  , cplaceFreq    = [(EMPTY, 1)]
  , cpassable     = True
  , cdefTile      = EMPTY_SET_LIT
  , cdarkCorTile  = FLOOR_ARENA_DARK
  , clitCorTile   = FLOOR_ARENA_LIT
  , cminStairDist = 30
  , cmaxStairsNum = 1
  , cstairFreq    = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Swirls of warm fog fill the air, the hiss of geysers sounding all around."
  }
outermost = shallowRogue
  { cname         = "Cave entrance"
  , cfreq         = [(CAVE_OUTERMOST, 100)]
  , cXminSize     = 40
  , cYminSize     = 21
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cactorCoeff   = 100  -- already animals start there; also, pity on the noob
  , cactorFreq    = filter ((/= MONSTER) . fst) $ cactorFreq rogue
  , citemNum      = 12 `d` 2  -- lure them in with loot; relatively consisten
  , citemFreq     = filter ((/= IK.TREASURE) . fst) $ citemFreq rogue
  , cminStairDist = 10  -- distance from the escape
  , cmaxStairsNum = 1  -- simplify at the start
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cdesc         = "This close to the surface, the sunlight still illuminates the dungeon."
  }
shallowRogue = rogue
  { cfreq         = [(CAVE_SHALLOW_ROGUE, 100)]
  , cXminSize     = 60
  , cYminSize     = 21
  , cmaxStairsNum = 1  -- simplify at the start
  , cdesc         = "The snorts and grunts of savage beasts can be clearly heard."
  }

-- * Overground "caves"; no story-wise limits wrt height and size

raid = rogue
  { cname         = "Typing den"
  , cfreq         = [(CAVE_RAID, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 6) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cdoorChance   = 1  -- make sure enemies not seen on turn 1
  , copenChance   = 0  -- make sure enemies not seen on turn 1
  , cactorCoeff   = 300  -- deep level with no kit, so slow spawning
  , cactorFreq    = [(ANIMAL, 100)]
  , citemNum      = 18  -- first tutorial mode, so make it consistent
  , citemFreq     = [ (IK.COMMON_ITEM, 100), (IK.S_CURRENCY, 500)
                    , (STARTING_WEAPON, 100) ]
  , cmaxStairsNum = 0
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mold spreads across the walls and scuttling sounds can be heard in the distance."
  }

mini = rogue
  { cname = "MiNI parking lot"
  , cfreq = [(CAVE_MINI, 1)]
  , cdesc = "Although it's supposed to be a parking lot, it doesn't look like one" 
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 6) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cdoorChance   = 1  -- make sure enemies not seen on turn 1
  , copenChance   = 0  -- make sure enemies not seen on turn 1
  , cactorCoeff   = 300  -- deep level with no kit, so slow spawning
  , cactorFreq    = [(ANIMAL, 100)]
  , citemNum      = 18  -- first tutorial mode, so make it consistent
  , citemFreq     = [ (IK.COMMON_ITEM, 100), (IK.S_CURRENCY, 500)
                    , (STARTING_WEAPON, 100) ]
  , cmaxStairsNum = 0
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  }

brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { cname         = "Sunny woodland"
  , cfreq         = [(CAVE_BRAWL, 1)]
  , cXminSize     = 60
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 5 + 5) 6
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 4 `d` 6
  , citemFreq     = [ (IK.COMMON_ITEM, 50), (STARTING_WEAPON, 100)
                    , (STARTING_ARMOR, 100) ]
  , cplaceFreq    = [(BRAWL, 1)]
  , cpassable     = True
  , cdefTile      = BRAWL_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "Sunlight falls through the trees and dapples on the ground."
  }
