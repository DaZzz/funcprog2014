import Task1
import Task6
import System.Environment

main = do
  [[c], n] <- getArgs
  print $ totalLength `fmap` (build c (read n))