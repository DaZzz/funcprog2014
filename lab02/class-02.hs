-- 1. Использование кортежей

-- 1
secondsToHms :: Integral a => a -> (a, a, a) 
secondsToHms seconds = (h, m, s)
  where 
    h = seconds `div` 3600
    m = (seconds `div` 60) `mod` 60
    s = seconds `mod` 60

-- 2
hmsToSeconds :: Integral a => (a, a, a) -> a
hmsToSeconds (h, m, s) = h * 3600 + m * 60 + s

-- 3
timeInterval :: Integral a => a -> a -> a -> a
timeInterval h m s = hmsToSeconds (h, m, s)

-- 4
segmentLength :: Floating a => (a, a) -> (a, a) -> a
segmentLength (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- 5
areaAndPerimeter :: Floating a => (a, a) -> (a, a) -> (a, a) -> (a, a)
areaAndPerimeter p1 p2 p3 = (area, perimeter)
  where 
    a = segmentLength p1 p2
    b = segmentLength p2 p3
    c = segmentLength p3 p1
    perimeter = a + b + c
    p = perimeter / 2
    area = sqrt (p * (p - a) * (p - b) * (p - c))

-- 2. Рекурсивная обработка списков.

-- 1
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if x `mod` 2 == 0 then 1 + (nEven xs) else nEven xs

-- 2
doubleElements :: Num a => [a] -> [a]
doubleElements [] = []
doubleElements (x:xs) = (x * 2 : (doubleElements xs))

-- 3
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if odd x then x : (fltOdd xs) else fltOdd xs

-- 4
notNegatives :: (Num a, Ord a) => [a] -> [a]
notNegatives [] = []
notNegatives (x : xs) = if x > 0 then x : notNegatives xs else notNegatives xs

-- 7
doubleEvens :: Integral a => [a] -> [a]
doubleEvens [] = []
doubleEvens (x : xs) = if even x then x * 2 : doubleEvens xs else x : doubleEvens xs

-- 6
shuffle :: [a] -> [a]
shuffle [] = []
shuffle [x] = []
shuffle (x1:x2:xs) = x2 : x1 : shuffle xs

-- 7
combinePlus :: [Integer] -> [Integer] -> [Integer]
combinePlus [] ys = ys
combinePlus xs [] = xs
combinePlus (x:xs) (y:ys) = x + y : combinePlus xs ys

-- 8
firstNthDescending :: Integer -> [Integer]
firstNthDescending n
  | n > 0 = n : firstNthDescending (n - 1)
  | otherwise = []

-- 9 
firstNthAscending :: Integer -> [Integer]
firstNthAscending n
  | n > 0 = firstNthAscending (n - 1) ++ [n]
  | otherwise = []

-- 10
zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = [] 
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- 3. Реализация аналогов стандартных функций.

-- 1
nth :: [a] -> Int -> a
nth [] _ = error "Index too large"
nth (x:_) 0 = x
nth (x:xs) i
  | i < 0 = error "Index less than zero"
  | otherwise = nth xs (i - 1)

-- 2
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) element
  | element == x = True
  | otherwise = contains xs element

-- 3
firstN :: [a] -> Int -> [a]
firstN [] _ = error "Index is too large"
firstN (x:xs) n
  | n < 0 = error "Index is less than zero"
  | n == 0 = []
  | otherwise = x : (firstN xs $ n - 1)

-- 4
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' t x = x : (replicate' (t - 1) x)

-- 5
concat' :: [a] -> [a] -> [a]
concat' [] y = y
concat' (x:xs) y = x : (concat' xs y)

-- 6
insertA :: [a] -> a -> [a]
insertA [] _ = []
insertA (x:xs) a = x : a : (insertA xs a)

-- 7
eqToFirstAndRest :: Eq a => [a] -> ([a], [a])
eqToFirstAndRest [] = ([], [])
eqToFirstAndRest (x:xs) = (eqTo (x:xs) x, notEqTo (x:xs) x)
  where
    eqTo [] _ = []
    eqTo (x:xs) a = if x == a then x : eqTo xs a else []
    notEqTo [] _ = []
    notEqTo (x:xs) a = if x /= a then (x:xs) else notEqTo xs a

-- 8
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' xs = gr : group' rest
  where
    firstGroupAndRest = eqToFirstAndRest xs
    gr = fst firstGroupAndRest
    rest = snd firstGroupAndRest

-- 9
enumerate :: [a] => [(Int, a)]
enumerate xs = enum' xs 0
  where
    enum' [] _ = []
    enum' (x:xs) i = (i, x) : enum' xs (i+1)

-- 10
removeEq :: Eq a => [a] -> [a]
removeEq [] = []
removeEq (x:xs) = x : removeEq (snd (eqToFirstAndRest (x:xs)))
-- Почему не работает это?
-- removeEq (x:xs) = x : removeEq $ snd $ eqToFirstAndRest (x:xs)


