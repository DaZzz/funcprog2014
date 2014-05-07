module Task4 where

import Control.Applicative
import System.Random

maxApp :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp a b = pure max <*> a <*> b

exampleMaxApp = do
  print $ maxApp (Just 2) (Just 4)
  print $ maxApp (Just 6) (Just 4)
  print $ maxApp (Just 6) (Nothing)
  print $ maxApp (Right 2) (Left "Not gonna happen")
  print $ maxApp (Right 2 :: Either String Int) (Right 5 :: Either String Int)
  print $ maxApp [1,2,3] [3,2,1]
  maxApp (randomRIO (1,10) :: IO Int) (randomRIO (1,10) :: IO Int) >>= print


