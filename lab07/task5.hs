import Data.Set as S
import Prelude as P
import Data.Array as A
import Criterion.Main

fibonacci :: [Integer]
fibonacci = takeWhile (<1000000) $ 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

fibonacciSet :: Set Integer
fibonacciSet = S.fromList fibonacci

fibonacciArray :: Array Int Integer
fibonacciArray = A.listArray (0, (length fibonacci) - 1) fibonacci

elemArray :: (Eq a, Num b, Ix b) => a -> Array b a -> Bool
elemArray x a = elem' x a begin
  where
    begin = fst $ bounds a
    end = snd $ bounds a
    elem' x a i
      | i == end = x == a ! i
      | otherwise = x == a ! i || elem' x a (i + 1)

main = do
  content <- readFile "numbers.txt"
  let numbers = P.map (\el -> read el ::Integer) $ words content
  defaultMain [
                bench "fib list" $ nf length (P.filter (flip elem fibonacci) numbers)
              , bench "fib set" $ nf length (P.filter (flip member fibonacciSet) numbers)
              , bench "fib array" $ nf length (P.filter (flip elemArray fibonacciArray) numbers)
              ]

