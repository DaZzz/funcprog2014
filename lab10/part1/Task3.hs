import Task1
import System.Environment

main = (totalLength . lines) `fmap` (head `fmap` getArgs >>= readFile) >>= print

