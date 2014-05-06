import System.Environment

maxInLine :: String -> Int
maxInLine l = maximum $ map (\w -> read w :: Int) (words l)

main = do
  [f] <- getArgs
  content <- readFile f
  let maxes = map maxInLine (lines content)
  print $ sum maxes

