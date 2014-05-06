import System.Environment 
import System.Directory
import System.IO

main = do
  [str, path] <- getArgs 

  (tmpPath, tmpHandle) <- openTempFile "." path
  hPutStr tmpHandle str

  oldContent <- readFile path
  hPutStrLn tmpHandle oldContent

  hClose tmpHandle
  removeFile path
  renameFile tmpPath path

