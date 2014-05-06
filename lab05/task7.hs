import System.Environment 
import System.Directory
import System.IO
import Data.Char
import System.Random
import Data.List

randomList :: Int -> Int -> IO [Int]
randomList a b = getStdGen >>= return . randomRs (a,b)  

main = do
  [f] <- getArgs
  w <- randomRIO(1, 10)
  h <- randomRIO(1, 10)
  rList <- randomList (-1000) (1000)
  let almostMatrix = take (w * h) rList
  let matrix = unfoldr (\xs -> if xs == [] then Nothing else Just (take w xs, drop w xs)) almostMatrix
  writeFile f (show matrix)

