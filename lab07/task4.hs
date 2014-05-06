import Data.Map as M
import Data.Set
import Data.Char
import System.IO
import Prelude as P

main = do
  content <- readFile "pushkin.txt"
  let ws = words content
  let f m key = insertWith (\_ b -> b+1) key 1 m
  let dict = M.toList $ P.foldl f M.empty ws
  mapM (\(k, v) -> putStrLn (k ++ " => " ++ (show v)) ) dict