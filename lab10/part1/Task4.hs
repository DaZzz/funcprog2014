module Task4 where

import Data.List

build :: Char -> Int -> Maybe [String]
build c 0 = Nothing
build c n = Just $ take n $ iterate (c:) []