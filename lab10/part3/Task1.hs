module Task1 where

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise = Left $ unwords ["You are dead with", show (left + n), show right]

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
  | abs ((right + n) - left) < 4 = Right (left, right + n)
  | otherwise = Left $ unwords
    ["You are dead with", show left, "birds on the left and", show (right + n), "on the right"]