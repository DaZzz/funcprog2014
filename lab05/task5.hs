import System.Environment 
import System.Directory
import System.IO
import Data.Char

main = do
  [f1, f2] <- getArgs
  content1 <- readFile f1
  writeFile f2 $ map toUpper content1