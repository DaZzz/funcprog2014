import Task1
import Task4
import System.Environment

main = do
  [c, n] <- getArgs
  print $ totalLength `fmap` (build (read c) (read n))

