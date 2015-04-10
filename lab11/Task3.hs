import Control.Monad.Writer

fact n = product [1..n]

sin' :: Double -> Integer -> Writer [(Double, Integer)] Double
sin' x 0 = tell [(x, 0)] >> return x
sin' x n = tell [(summand, n)] >> (+ summand) `fmap` (sin' x (n-1))
  where
    summand = (-1.0)^n / (fromIntegral (fact (2*n + 1))) * (x^(2*n + 1))

cos' :: Double -> Integer -> Writer [(Double, Integer)] Double
cos' x 0 = tell [(x, 0)] >> return 1
cos' x n = tell [(summand, n)] >> (+ summand) `fmap` (cos' x (n-1))
  where
    summand = (-1.0)^n / (fromIntegral (fact (2*n))) * (x^(2*n))