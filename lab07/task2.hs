import Data.Set
import System.IO

main = do
  content <- readFile "pushkin.txt"
  putStr "Различных слов: "
  putStrLn $ show $ size $ fromList $ words content