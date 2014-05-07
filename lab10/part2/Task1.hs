module Task1 where

reduce :: Integral a => a -> a
reduce a
  | a `mod` 3 == 0 = 0
  | a `mod` 2 /= 0 = a^2
  | otherwise = a^3