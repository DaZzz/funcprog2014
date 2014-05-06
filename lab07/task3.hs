import Data.Set
import System.IO

main = do
  content1 <- readFile "pushkin.txt"
  content2 <- readFile "gogol.txt"
  let set1 = fromList $ words content1
  let set2 = fromList $ words content2
  putStr "Из первого файла во втором содержатся "
  putStr $ show $ size $ intersection set1 set2
  putStrLn " слов"