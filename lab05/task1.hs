import System.Environment

main = do 
  [f] <- getArgs
  writeFile f $ unlines $ replicate 1000 "Привет, мир"