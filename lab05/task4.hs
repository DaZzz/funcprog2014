import System.Environment 
import System.Directory
import System.IO

main = do
  [f1, f2] <- getArgs
  (tmpPath, tmpHandle) <- openTempFile "." f1
  content1 <- readFile f1
  content2 <- readFile f2
  hPutStr tmpHandle $ unlines $ zipWith (++) (lines content1) (lines content2)
  hPutStrLn tmpHandle $ unlines $ drop (length (lines content2)) (lines content1)
  hClose tmpHandle
  removeFile f1
  renameFile tmpPath f1