import System.Random
import Control.Monad(when)
import Data.Set

main = do
  putStrLn "========== Быки и Коровы =========="
  putStrLn "[Хозяин таверны]: Смотрите, кто к нам пришел! Ну, присаживайся..."
  newRound

pick :: [a] -> IO (a, [a])
pick [] = error "Can't pick from empty list"
pick xs = do
  i <- randomRIO (0, (length xs) - 1)
  return ((xs !! i), (take (i) xs) ++ (drop (i+1) xs))

shuffle :: [a] -> IO [a]
shuffle [] = do return []
shuffle xs = do
  (picked, rest) <- pick xs
  shuffled <- shuffle (rest)
  return (picked : shuffled)

generateNumber :: IO String
generateNumber = do
  shuffled <- shuffle [1..9]
  return $ concat $ Prelude.map (\a -> show a) $ take 4 $ shuffled

checkAnswer :: String -> String -> String
checkAnswer a b =
  let
    bulls = length $ Prelude.filter (\(x, y) -> x == y) (zip a b)
    cows = (size $ intersection (fromList a) (fromList b)) - bulls
    result = (replicate bulls 'Б') ++ (replicate cows 'K')
  in
    if result == ""
      then "Хех! Всё мимо! ¯\\_(ツ)_/¯"
      else result

newRound :: IO ()
newRound = do
  secretNumber <- generateNumber
  putStrLn "[Хозяин таверны]: Я загадал число! Отгадывай!"
  askForNumber secretNumber

askForNumber :: String -> IO ()
askForNumber secretNumber = do
  numberString <- getLine
  let answer = checkAnswer secretNumber numberString
  if answer == "ББББ"
    then do {putStrLn "[Хозяин таверны]: Ты отгадал! Замечательно! Давай еще раз!"; newRound}
    else putStrLn $ "[Хозяин таверны]: Хаха! Ты был близок! Вот твои коровы с быками: " ++ answer
  askForNumber secretNumber

