import Data.Char
import Data.Maybe
import Data.List
import System.Environment

type Transformation = (Integer -> Integer)

operations :: [String]
operations = ["summand", "multiplier", "divisor"]

isInteger :: String -> Bool
isInteger = all isDigit

parseTransformation :: String -> Maybe Transformation
parseTransformation s = if isValid then Just t else Nothing
  where
    args = words s

    isValid =
      length args == 3
      && elem (args !! 0) operations
      && (args !! 1) == "="
      && isInteger (args !! 2)

    [o, _, n] = args

    t = case o of
      "summand" -> (+ (read n))
      "multiplier" -> (* (read n))
      "divisor" -> (`div` (read n))

loadTransformations :: FilePath -> IO [Transformation]
loadTransformations f = do
  content <- readFile f
  return $ map fromJust $ filter isJust $ map parseTransformation $ lines content

loadNumbers :: FilePath -> IO [Integer]
loadNumbers f = do
  content <- readFile f
  return $ map read $ words content

performTransformations :: [Transformation] -> Integer -> Integer
performTransformations ts = foldl' (.) id (reverse ts)

main = do
  [f1, f2] <- getArgs
  ts <- loadTransformations f1
  numbers <- loadNumbers f2
  print $ map (performTransformations ts) numbers
