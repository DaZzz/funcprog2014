import System.Environment 
import System.IO
import Data.List
import System.Directory
import Text.Regex

courseGroupCompare [_,_,c1,g1] [_,_,c2,g2] = compare (c1, g1) (c2, g2)

printGroup :: [[[Char]]] -> IO ()
printGroup gr = do
  let c = (head gr) !! 2
  let g = (head gr) !! 3
  writeFile (c ++ "_" ++ g ++ ".txt") $ unlines $ map (\[name, _, _, _] -> name) gr

printAmount :: [[[Char]]] -> IO ()
printAmount gr = do
  let c = (head gr) !! 2
  let g = (head gr) !! 3
  putStrLn $ "In group " ++ c ++ "." ++ g ++ " " ++ (show (length gr)) ++ " students"

main = do 
  [f, c', g'] <- getArgs
  content <- readFile f
  let students = map (splitRegex (mkRegex ",")) $ lines content 
  let sorted = sortBy courseGroupCompare students
  let grouped = groupBy (\[_,_,c1,g1] [_,_,c2,g2] -> c1 == c2 && g1 == g2) sorted 
  mapM (\g -> printAmount g) grouped
  mapM (\g -> printGroup g) grouped

  let ages = map (\(_:a:_) -> read a ::Int) $ head $ dropWhile (\([_,_,c,g]:_) -> c /= c' && g /=  g') grouped
  putStr $ "Average age in group " ++ c' ++ "." ++ g' ++ " is "
  print $ (fromIntegral (foldl (+) 0 ages)) / (fromIntegral (length ages))
  