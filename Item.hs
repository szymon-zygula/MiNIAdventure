module Item where

import Data.Binary
import Data.Set as S
import Data.List as L
import Data.Maybe
import Data.Char
import Control.Monad

import Display
import Geometry
import Random

data Item = Item
             { icount  :: Int,
               itype   :: ItemType,   
               iletter :: Maybe Char }  -- inventory identifier
  deriving Show

data ItemType =
   Ring
 | Scroll
 | Potion
 | Wand
 | Amulet
 | Gem
 | Gold
 deriving (Eq, Show)

instance Binary Item where
  put (Item icount itype iletter) = put icount >> put itype >> put iletter
  get = liftM3 Item get get get

instance Binary ItemType where
  put Ring   = putWord8 0
  put Scroll = putWord8 1
  put Potion = putWord8 2
  put Wand   = putWord8 3
  put Amulet = putWord8 4
  put Gem    = putWord8 5
  put Gold   = putWord8 6
  get = do
          tag <- getWord8
          case tag of
            0 -> return Ring
            1 -> return Scroll
            2 -> return Potion
            3 -> return Wand
            4 -> return Amulet
            5 -> return Gem
            6 -> return Gold

itemFrequency :: Frequency ItemType
itemFrequency =
  Frequency
  [
    (10, Gold),
    (3, Gem),
    (2, Ring),
    (4, Scroll),
    (2, Wand),
    (1, Amulet),
    (4, Potion)
  ]

itemQuantity :: Int -> ItemType -> Rnd Int
itemQuantity n Gold = (2 * n) *~ d 8
itemQuantity _ _    = return 1

-- | Generate an item.
newItem :: Int -> Frequency ItemType -> Rnd Item
newItem n ftp =
  do
    tp <- frequency ftp
    nr <- itemQuantity n tp
    return (Item nr tp Nothing)

-- | Assigns a letter to an item, for inclusion
-- in the inventory of the player. Takes a remembered
-- letter and a starting letter.
assignLetter :: Maybe Char -> Char -> [Item] -> Maybe Char
assignLetter r c is =
    case r of
      Just l | l `L.elem` free -> Just l
      _ -> listToMaybe free
             
  where
    current    = S.fromList (concatMap (maybeToList . iletter) is)
    allLetters = ['a'..'z'] ++ ['A'..'Z']
    candidates = take (length allLetters) (drop (fromJust (findIndex (==c) allLetters)) (cycle allLetters))
    free       = L.filter (\x -> not (x `member` current)) candidates

cmpLetter :: Char -> Char -> Ordering
cmpLetter x y = compare (isUpper x, toLower x) (isUpper y, toLower y)

cmpLetter' :: Maybe Char -> Maybe Char -> Ordering
cmpLetter' Nothing  Nothing   = EQ
cmpLetter' Nothing  (Just _)  = GT
cmpLetter' (Just _) Nothing   = LT
cmpLetter' (Just l) (Just l') = cmpLetter l l'

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y = case cmp x y of
                  LT  ->  y
                  _   ->  x

maxLetter = maxBy cmpLetter

mergeLetter :: Maybe Char -> Maybe Char -> Maybe Char
mergeLetter = mplus

letterRange :: [Char] -> String
letterRange xs = sectionBy (sortBy cmpLetter xs) Nothing
  where
    succLetter c d = ord d - ord c == 1

    sectionBy []     Nothing                  = ""
    sectionBy []     (Just (c,d))             = finish (c,d)
    sectionBy (x:xs) Nothing                  = sectionBy xs (Just (x,x))
    sectionBy (x:xs) (Just (c,d)) | succLetter d x
                                              = sectionBy xs (Just (c,x))
                                  | otherwise
                                              = finish (c,d) ++ sectionBy xs (Just (x,x))

    finish (c,d) | c == d         = [c]
                 | succLetter c d = [c,d]
                 | otherwise      = [c,'-',d]

letterLabel :: Maybe Char -> String
letterLabel Nothing  = "    "
letterLabel (Just c) = c : " - "

viewItem :: ItemType -> (Char, Attr -> Attr)
viewItem Ring   = ('=', id)
viewItem Scroll = ('?', id)
viewItem Potion = ('!', id)
viewItem Wand   = ('/', id)
viewItem Gold   = ('$', setBold . setFG yellow)
viewItem Gem    = ('*', setFG red)
viewItem _      = ('~', id)

-- | Adds an item to a list of items, joining equal items.
-- Also returns the joined item.
joinItem :: Item -> [Item] -> (Item,[Item])
joinItem i is = case findItem (\ j -> itype i == itype j) is of
                  Nothing     -> (i, i : is)
                  Just (j,js) -> let n = i { icount = icount i + icount j,
                                             iletter = mergeLetter (iletter j) (iletter i) }
                                 in (n, n : js)

-- | Finds an item in a list of items.
findItem :: (Item -> Bool) -> [Item] -> Maybe (Item, [Item])
findItem p is = findItem' [] is
  where
    findItem' acc []     = Nothing
    findItem' acc (i:is)
      | p i              = Just (i, reverse acc ++ is)
      | otherwise        = findItem' (i:acc) is

objectItem :: Int -> ItemType -> String
objectItem 1 Ring   = "a ring"
objectItem n Ring   = show n ++ " rings"
objectItem 1 Scroll = "a scroll"
objectItem n Scroll = show n ++ " scrolls"
objectItem 1 Potion = "a potion"
objectItem n Potion = show n ++ " potions"
objectItem 1 Wand   = "a wand"
objectItem n Wand   = show n ++ " wands"
objectItem 1 Amulet = "an amulet"
objectItem n Amulet = show n ++ " amulets"
objectItem 1 Gem    = "a gem"
objectItem n Gem    = show n ++ " gems"
objectItem 1 Gold   = "a gold piece"
objectItem n Gold   = show n ++ " gold pieces"
