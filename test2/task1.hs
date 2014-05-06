import System.Environment

main = do
  [n, m, f] <- getArgs
  let line = replicate (read m :: Int) '*'
  let lineList = replicate (read  n :: Int) line
  writeFile f $ unlines lineList
