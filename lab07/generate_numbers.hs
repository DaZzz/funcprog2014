import System.Random
import System.Environment
import System.IO

randomList :: Int -> Int -> IO [Int]
randomList a b = getStdGen >>= return . randomRs (a,b)

main = do
  [f] <- getArgs
  rList <- randomList 1 1000000
  let s = unwords . (map show) $ take 10000 $ rList
  writeFile f s