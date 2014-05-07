module Task5 where

import Control.Applicative
import System.Random

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max c $ max a b

maxApp :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp a b c = pure max3 <*> a <*> b <*> c

exampleMaxApp = do
  print $ maxApp (Just 2) (Just 4) (Just 8)
  print $ maxApp (Just 6) (Just 4) (Just 2)
  print $ maxApp (Just 6) (Nothing) (Just 2)
  print $ maxApp (Right 2) (Left "Not gonna happen") (Right 3)
  print $ maxApp (Right 2 :: Either String Int) (Right 5 :: Either String Int) (Right 3 :: Either String Int)
  print $ maxApp [1,2,3] [3,2,1] [2..5]
  maxApp (randomRIO (1,10) :: IO Int) (randomRIO (1,10) :: IO Int) (randomRIO (1,10) :: IO Int) >>= print