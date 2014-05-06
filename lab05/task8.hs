import System.Environment 
import System.IO
import Data.List


sumMainDiagonal :: [[Int]] -> Int
sumMainDiagonal m = foldl (+) 0 (map (\(a, b) -> a!!b) (zip m [0..]))

main = do
  [f] <- getArgs
  sMatrix <- readFile f
  let matrix = read sMatrix :: [[Int]]
  putStrLn $ show $ sumMainDiagonal matrix
