import Task2

lineToPair :: String -> (Int, Int)
lineToPair s = (read a :: Int, read b :: Int)
  where a:b:_ = words s

main = do
  content <- readFile "f1.txt"
  let pair = map lineToPair $ lines content
  putStr "List: "
  print $ listOfPairsToList pair
  putStr "Maybe: "
  print $ listOfPairsToMaybe pair
  putStr "Either: "
  print $ listOfPairsToEither pair
  putStr "IO: "
  (listOfPairsToIO pair) >>= print
