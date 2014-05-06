module Drunkard where

import System.Random
import System.IO  

-- 1
data Suit = Clubs | Diamonds | Spades | Hearts
  deriving(Eq, Ord, Read, Show, Bounded, Enum)

data Rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving(Eq, Ord, Read, Show, Bounded, Enum)

type Card = (Rank, Suit)
type Pack = [Card]

-- 2
sameSuit :: Card -> Card -> Bool
sameSuit (_, s1) (_, s2) = s1 == s2

-- 3
beats :: Card -> Card -> Bool
beats (r1, _) (r2, _) = r1 > r2

-- 4
sameRank :: Card -> Card -> Bool
sameRank (r1, _) (r2, _) = r1 == r2

makeStep :: (Pack, Pack) -> (Pack, Pack)
makeStep (pack1, pack2) = makeStep' pack1 pack2 []
  where
    makeStep' [] [] _ = ([], [])
    makeStep' pack1 [] bank = (pack1 ++ bank, [])
    makeStep' [] pack2 bank = ([], pack2 ++ bank)
    makeStep' (c1:cs1) (c2:cs2) bank
      | c1 `sameRank` c2 = makeStep' cs1 cs2 (c1:c2:bank)
      | c1 `beats` c2 = (cs1 ++ (c1:c2:bank), cs2)
      | c2 `beats` c1 = (cs1, cs2 ++ (c1:c2:bank))

-- 5
stepsTillEnd :: (Pack, Pack) -> Int
stepsTillEnd (pack1, pack2) = stepsTillEnd' (pack1, pack2) 0
  where
    stepsTillEnd' (_, []) i = i
    stepsTillEnd' ([], _) i = i 
    stepsTillEnd' (pack1, pack2) i = stepsTillEnd' (makeStep (pack1, pack2)) (i+1)

-- 6
readCards :: FilePath -> IO Pack
readCards f = do 
  handle <- openFile f ReadMode
  contents <- hGetContents handle
  return (read contents)

-- 7
zipEach :: [a] -> [b] -> [(a, b)]
zipEach _ [] = []
zipEach xs (y:ys) = (zip xs (replicate (length xs) y)) ++ zipEach xs ys

pick :: [a] -> IO (a, [a])
pick [] = error "Can't pick from empty list"
pick xs = do
  i <- randomRIO (0, (length xs) - 1)
  return ((xs !! i), (take (i) xs) ++ (drop (i+1) xs))

shuffle :: [a] -> IO [a]
shuffle [] = do return []
shuffle xs = do
  (picked, rest) <- pick xs
  shuffled <- shuffle (rest)
  return (picked : shuffled)

shuffledPack :: IO Pack
shuffledPack = shuffle (zipEach [Six ..] [Clubs ..])

splitByLength :: Int -> [a] -> [[a]]
splitByLength _ [] = []
splitByLength l xs = (take l xs) : (splitByLength l (drop l xs))

drawCards :: Int -> IO [Pack]
drawCards nPlayers = do
  cards <- shuffledPack
  return (splitByLength ((length cards) `div` nPlayers) cards)


