import Data.List
import Data.Char
import Numeric

-- 1. Простейшие задачи на применение функций map и filter.

-- 1
doubleElements :: [Integer] -> [Integer]
doubleElements = map (2*)

-- 2
doubleEvenElements :: [Integer] -> [Integer]
doubleEvenElements = map (\a -> if even a then 2*a else a)

-- 3
zeroizeOddElements :: [Integer] -> [Integer]
zeroizeOddElements = map (\a -> if odd a then 0 else a)

-- 4 
deleteMoreThanK :: Integer -> [Integer] -> [Integer]
deleteMoreThanK k = filter (<= k)

-- 5
negatives :: [Integer] -> [Integer]
negatives = filter (<0)

-- 6
deletePositiveEven :: [Integer] -> [Integer]
deletePositiveEven = filter (\a -> not (even a && a > 0) )

-- 7
getQuarter :: (Double, Double) -> Integer
getQuarter (x, y)
  | x > 0 && y > 0 = 1
  | x > 0 && y < 0 = 2
  | x < 0 && y < 0 = 3
  | x < 0 && y > 0 = 4
  | otherwise = -1

filterCoords :: Integer -> [(Double, Double)] -> [(Double, Double)]
filterCoords n = filter (\p -> getQuarter p == n)

-- 8
cartesianToPolar :: RealFloat a => (a, a) -> (a, a)
cartesianToPolar (x, y) = (r, phi)
  where 
    r = sqrt $ x^2 + y^2
    phi = atan2 y x

cartesianToPolarList :: RealFloat a => [(a,a)] -> [(a,a)]
cartesianToPolarList = map cartesianToPolar

-- 9
capitalize :: [String] -> [String]
capitalize = map $ map toUpper 

-- 10
fixedLengthStrings :: Int -> [String] -> [String]
fixedLengthStrings n = filter ((==n).length)

-- 2. Формирование числовых последовательностей (iterate).

-- 1
naturalNumbersList :: [Integer]
naturalNumbersList = iterate (+1) 0

-- 2
evenNumbers :: [Integer]
evenNumbers = iterate (+2) 0

-- 3
superSequence :: (Fractional a, Num a) => [a]
superSequence = iterate (\a -> (1+a) / 2) 1

-- 4
englishAlphabet :: [Char]
englishAlphabet = take 26 (iterate (chr.(+1).ord) 'a')

-- 5 
decToBin :: Int -> String
decToBin n = map intToDigit (reverse (unfoldr (\a -> if a == 0 then Nothing else Just (a `mod` 2, a `div` 2)) n))

binNumbers :: Int -> [String]
binNumbers n = map decToBin [2^(n - 1)..2^n - 1]

-- 3. Группировка списков

-- 1 
groupNumbersAndLetters :: String -> [String]
groupNumbersAndLetters = groupBy (\a b -> isDigit a && isDigit b || isLetter a && isLetter b)

-- 2
type Point = (Double, Double)

groupSameQuarter :: [Point] -> [[Point]]
groupSameQuarter = groupBy (\a b -> (getQuarter a) == (getQuarter b))

-- 3 
splitByLength :: Eq a => Int -> [a] -> [[a]]
splitByLength n = unfoldr (\l -> if l == [] then Nothing else Just ((take n l), (drop n l)))

-- 4
splitByLengths :: Eq a => Int -> Int -> [a] -> [[a]]
splitByLengths n m = unfoldr (\l -> if l == [] then Nothing else Just ((take n l), (drop (n-m) l)))

-- 5
longestSublist :: (Eq a, Num a) => [a] -> Int
longestSublist = foldl (\n l -> max n (length l)) (-1) . group

-- 4. Разные задачи.

-- 1
countNumbers :: String -> Int
countNumbers = length . filter (\(x:xs) -> isDigit x) . groupBy (\a b -> isDigit a && isDigit b)

-- 2
fibonacci :: [Int]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

sumFibonacciPredicate :: (Int -> Bool) -> Int -> Int -> Int
sumFibonacciPredicate p a b = foldr (+) 0 (filter p $ takeWhile (<b) $ dropWhile (<a) fibonacci)

-- 3
mostUsable :: Int -> String -> [String]
mostUsable n = (take n) . map head  . sortBy (\a b -> compare (length b) (length a)) . group . sort . words 

-- 4
localMaximums :: Ord a => [a] -> [a]
localMaximums (x:y:xs) = map (\(a,b,c) -> b) $ filter (\(a,b,c) -> a < b && b > c) $ zip3 (x:y:xs) (y:xs) xs
localMaximums _ = []

-- 5
duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = x:x:duplicateElements xs