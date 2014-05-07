module Task6 where

import Data.List

build :: Char -> Int -> Either String [String]
build c n
  | c == 'x' = Left "Роспотребнадзор запрещает создавать строки из символа x."
  | n == 0 = Left "n = 0"
  | n > 100 = Left "n > 100"
  | otherwise = Right $ take n $ iterate (c:) []