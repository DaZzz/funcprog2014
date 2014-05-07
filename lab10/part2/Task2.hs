module Task2 where

import Task1
import System.Random

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n f = foldr fmap f (replicate n reduce)

listOfPairsToList :: [(Int, Int)] -> [Int]
listOfPairsToList l = zipWith (+) (fst a) (snd a)
  where a = unzip l

listOfPairsToMaybe :: [(Int, Int)] -> Maybe Int
listOfPairsToMaybe l = if even summa then Just summa else Nothing
  where summa = sum $ listOfPairsToList l

listOfPairsToEither :: [(Int, Int)] -> Either String Int
listOfPairsToEither l = if even summa then Right summa else Left "Sorry, bratishka, ne poluchilos"
  where summa = sum $ listOfPairsToList l

listOfPairsToIO :: [(Int, Int)] -> IO Int
listOfPairsToIO l = randomRIO (1, sum $ listOfPairsToList l)
