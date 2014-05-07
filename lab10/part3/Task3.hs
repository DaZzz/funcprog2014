module Task3 where

import Task2
import Data.List

toString :: [Student] -> String
toString students = unlines $ map (\(Student n a g) -> intercalate "\n" [n, show a, g]) students

writeToFile :: FilePath -> [Student] -> IO ()
writeToFile f students = writeFile f (toString students)

weirdConcat :: FilePath -> FilePath -> FilePath -> IO ()
weirdConcat f1 f2 f3 =
  loadFromFile f1 >>=
    \s1 -> loadFromFile f2 >>=
      \s2 -> writeToFile f3 (s1 ++ s2)
