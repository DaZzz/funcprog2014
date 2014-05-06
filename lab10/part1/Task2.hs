import Task1
import System.Environment

main = totalLength `fmap` getArgs >>=  print