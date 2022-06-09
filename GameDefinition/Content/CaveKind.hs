-- | Definitions of of cave kinds. Every level in the game is an instantiated
-- cave kind.
module Content.CaveKind
  ( pattern CAVE_PARKING_LOT
  , pattern CAVE_MINI_LABORATORY
  , pattern CAVE_SERVER_ROOM
  , groupNamesSingleton
  , groupNames
  , content
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

import Content.ItemKindActor
import Content.PlaceKind hiding (content, groupNames, groupNamesSingleton)
import Content.TileKind hiding (content, groupNames, groupNamesSingleton)


groupNamesSingleton :: [GroupName CaveKind]
groupNamesSingleton = []

groupNames :: [GroupName CaveKind]
groupNames =
    [ CAVE_PARKING_LOT
    , CAVE_SERVER_ROOM
    , CAVE_MINI_LABORATORY
    , CAVE_ROGUE ]

pattern CAVE_PARKING_LOT :: GroupName c
pattern CAVE_PARKING_LOT = GroupName "caveParkingLot"
pattern CAVE_MINI_LABORATORY :: GroupName c
pattern CAVE_MINI_LABORATORY = GroupName "caveMiniLaboratory"
pattern CAVE_SERVER_ROOM :: GroupName c
pattern CAVE_SERVER_ROOM = GroupName "caveServerRoom"
pattern CAVE_ROGUE :: GroupName c
pattern CAVE_ROGUE = GroupName "caveRogue"

content :: [CaveKind]
content =
    [ parkingLot
    , miniLaboratory
    , serverRoom
    , rogue ]

-- * Underground caves; most of mediocre height and size

rogue :: CaveKind
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
  , cactorFreq    = []
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
  }

parkingLot :: CaveKind
parkingLot = rogue
  { cname = "MiNI parking lot"
  , cfreq = [(CAVE_PARKING_LOT, 1)]
  , cdesc = "Although it's supposed to be a parking lot, it doesn't look like one" 
  , cXminSize     = 75
  , cYminSize     = 21
  , ccellSize     = DiceXY (4 `d` 4 + 12) 12
  , cminPlaceSize = DiceXY (4 `d` 2 + 8) 10
  , cmaxPlaceSize = DiceXY 32 40
  , cdarkOdds     = 0
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , cactorCoeff   = 300
  , cactorFreq    = [(CAR, 100)]
  , citemNum      = 50
  , citemFreq     = [ (IK.EXPLOSIVE, 2000), (IK.COMMON_ITEM, 5), (STARTING_WEAPON, 5), (IK.VALUABLE, 5) ]
  , cmaxStairsNum = 2
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cstairFreq    = [ (GATED_CLOSED_STAIRCASE, 50)
                    , (GATED_OPEN_STAIRCASE, 50)
                    , (GATED_TINY_STAIRCASE, 1) ]
  , cminStairDist = 15
  , cstairAllowed = []
  }

miniLaboratory :: CaveKind
miniLaboratory = rogue
  { cname = "MiNI laboratory"
  , cfreq = [(CAVE_MINI_LABORATORY, 1)]
  , cdesc = "Laboratory full of computers for writing useful programs" 
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 6) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , cactorCoeff   = 300
  , cactorFreq    = [(COMPUTER, 200), (SERVER, 25)]
  , citemNum      = 50
  , citemFreq     = [ (IK.EXPLOSIVE, 1), (IK.COMMON_ITEM, 1), (STARTING_WEAPON, 1), (IK.VALUABLE, 1) ]
  , cmaxStairsNum = 1
  , cescapeFreq   = []
  , cminStairDist = 15
  , cstairFreq    = [ (GATED_TINY_STAIRCASE, 100) ]
  , cstairAllowed = []
  }

serverRoom :: CaveKind
serverRoom = rogue
  { cname = "MiNI server room complex"
  , cfreq = [(CAVE_SERVER_ROOM, 1)]
  , cdesc = "Server room complex full of servers for performing useful calculations" 
  , cXminSize     = 65
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 5 + 7) 8
  , cminPlaceSize = DiceXY (3 `d` 3 + 5) 6
  , cmaxPlaceSize = DiceXY 20 24
  , cdarkOdds     = 0
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , cactorCoeff   = 300
  , cactorFreq    = [(COMPUTER, 20), (SERVER, 500)]
  , citemNum      = 50
  , citemFreq     = [ (IK.EXPLOSIVE, 5), (IK.COMMON_ITEM, 2), (STARTING_WEAPON, 2), (IK.VALUABLE, 2) ]
  , cmaxStairsNum = 2
  , cescapeFreq   = []
  , cminStairDist = 15
  , cstairFreq    = [ (GATED_CLOSED_STAIRCASE, 50)
                    , (GATED_OPEN_STAIRCASE, 50)
                    , (GATED_TINY_STAIRCASE, 1) ]
  , cstairAllowed = []
  }
