import System.Environment 
import System.Directory
import System.IO
import Data.Char

main = do
  [f] <- getArgs
  content <- readFile f
  let numbers = map (\str -> map (\num -> read num ::Int) (words str)) (lines content)
  putStrLn $ show $ length $ filter (\[a, b] -> a > b) numbers
  putStrLn $ show $ foldl (+) 0 (map (!!1) numbers)
  