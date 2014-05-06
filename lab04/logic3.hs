module Logic3 where

-- 1
data Logic3 = T | U | F 
  deriving (Read, Show, Eq, Ord, Bounded, Enum)

-- 2
not3 :: Logic3 -> Logic3
not3 T = U 
not3 U = F
not3 F = T

and3 :: Logic3 -> Logic3 -> Logic3
and3 F _ = F
and3 _ F = F
and3 U _ = U
and3 _ U = U
and3 _ _ = T

or3 :: Logic3 -> Logic3 -> Logic3
or3 T _ = T
or3 _ T = T
or3 U _ = U
or3 _ U = U
or3 _ _ = F

-- 3
checkAlwaysTrue :: [Logic3]
checkAlwaysTrue = map (\x -> (not3 x) `or3` (not3 (not3 x)) `or3` x ) [T, U, F]

-- 4
and' :: [Logic3] -> Logic3
and' = foldr and3 T

or' :: [Logic3] -> Logic3
or' = foldr or3 F

any' :: (a -> Logic3) -> [a] -> Logic3
any' f = or' . (map f)

all' :: (a -> Logic3) -> [a] -> Logic3
all' f = and' . (map f)

isPrime :: Int -> Logic3
isPrime x
  | x > 7 = U
  | elem x [2,3,5,7] = T
  | otherwise = F




