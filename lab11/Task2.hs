import Data.Char
import Data.Maybe
import Data.List
import System.Environment
import Control.Monad.Reader
import qualified Data.Map as M

type Configuration = M.Map String Integer

defaultConfig :: Configuration
defaultConfig = M.fromList [("multiplier", 1), ("summand", 0), ("divisor", 1)]

parseConfigLine :: String -> (String, Integer)
parseConfigLine s = (a, read b)
  where
    [a,_,b] = words s

loadConfig :: FilePath -> IO Configuration
loadConfig f = do
  content <- readFile f
  let cs = map parseConfigLine $ lines content
  return $ foldl' (\config (k, v) -> M.insert k v config) defaultConfig cs

performTransformations :: Configuration -> Integer -> Integer
performTransformations c n = (n + s) * m `div` d
  where
    s = fromJust $ M.lookup "summand" c
    m = fromJust $ M.lookup "multiplier" c
    d = fromJust $ M.lookup "divisor" c


loadNumbers :: FilePath -> IO [Integer]
loadNumbers f = do
  content <- readFile f
  return $ map read $ words content

main = do
  [f1, f2] <- getArgs
  config <- loadConfig f1
  numbers <- loadNumbers f2
  print $ map (performTransformations config) numbers