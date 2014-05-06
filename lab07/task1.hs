import Data.IntSet
import System.IO

main = do
  content <- readFile "numbers.txt"
  let nums = Prelude.map (\n -> read n :: Int) $ words content
  putStr "Различных чисел: "
  putStrLn $ show $ size $ fromList nums