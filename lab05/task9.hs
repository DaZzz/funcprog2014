import System.Environment 
import System.IO
import Data.List
import System.Directory


justify :: String -> String
justify s = intercalate (replicate spaces ' ') $ words' ++ words''
  where
    lengthWithNoSpaces = length $ concat $ words s
    spaces = (50 - lengthWithNoSpaces) `div` ((length (words s)) - 1)
    extraSpaces = (50 - lengthWithNoSpaces) `mod` ((length (words s)) - 1)
    words' = take extraSpaces $ map (++" ") $ words s
    words'' = drop extraSpaces $ words s


main = do 
  [f] <- getArgs
  h <- openFile f ReadMode
  content <- hGetContents h
  let result = unlines $ map justify $ lines content
  (tF, tH) <- openTempFile "." f
  hPutStr tH result
  hClose tH
  hClose tH
  removeFile f
  renameFile tF f