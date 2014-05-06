import System.Environment

main = do
  [f] <- getArgs 
  content <- readFile f
  putStrLn $ show $ length $ lines content
