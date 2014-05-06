module Hull where

import Data.Fixed
import Data.List
import Data.Ord
import Data.Tuple
import System.IO

-- 1
type Point = (Double, Double)

-- 2
data Direction = R | L | M deriving (Read, Show, Bounded, Enum, Eq, Ord)

-- 3
getDirection :: (Point, Point, Point) -> Direction
getDirection ((x0, y0), (x1, y1),  (x2, y2)) 
  | a*x2 + b*y2 + c < 0 = R
  | a*x2 + b*y2 + c > 0 = L
  | otherwise = M
  where
    a = y0 - y1 
    b = -(x0 - x1) 
    c = -(a * x0 + b * y0)

getDirections :: [Point] -> [Direction]
getDirections (x:y:xs) = map getDirection (zip3 (x:y:xs) (y:xs) xs)

-- 4
cartesianToPolar :: Point -> (Double, Double)
cartesianToPolar (x, y) = (r, phi)
  where 
    r = sqrt $ x^2 + y^2
    phi = ((atan2 y x) + 2*pi) `mod'` (2*pi)


polarToCartesian :: Point -> Point
polarToCartesian (r, phi) = (r * (cos phi), r * (sin phi))


sortBySecond :: [(Double, Double)] -> [(Double, Double)]
sortBySecond = sortBy $ comparing swap


sortByAngle :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
sortByAngle (_, phi) = sortBy $ comparing (\(_, phi1) -> (phi1 + (2*pi - phi)) `mod'` (2*pi))


sortGraham :: [Point] -> [(Double, Double)]
--sortGraham l = (head sortedY) : ((map polarToCartesian . sortByAngle (cartesianToPolar (head sortedY)) . map cartesianToPolar) (tail sortedY))
--  where 
--    sortedY = sortBySecond l

sortGraham l = h : (( map Co . sortBySecond . map cartesianToPolar . map moveToCenter) t) 
  where 
    (x0, y0) = (head sortedY)
    t = (tail sortedY)
    moveToCenter (x, y) = (x - x0, y - y0)
    moveFromCenter (x, y) = (x + x0, y + y0)
    sortedY = sortBySecond l

allLeft :: [Point] -> Bool
allLeft ps = all (==L) (getDirections ps)


graham :: [Point] -> [Point]
graham points = grahamIter toCheck accumulator
  where
    grahamIter [] acc = acc
    grahamIter (x:xs) acc = 
      if allLeft (x:acc) 
      then grahamIter xs (x:acc) 
      else grahamIter (x:xs) (tail acc)

    toCheck = drop 2 (sortGraham points)
    accumulator = reverse (take 2 (sortGraham points))


toIntPoint :: Point -> (Int, Int)
toIntPoint (a, b) = (round a, round b)


-- 5
grahamFromFile :: FilePath -> FilePath -> IO ()
grahamFromFile fromF toF = do
  handle <- openFile fromF ReadMode
  contents <- hGetContents handle
  writeFile toF (show (map toIntPoint (graham (read contents))))







