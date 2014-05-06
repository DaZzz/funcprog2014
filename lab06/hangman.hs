import System.Random
import Control.Monad(when)
import Data.Set
import System.IO

states = [
    [
    "         ",
    "         ",
    "         ",
    "         ",
    "         ",
    "         ",
    "         ",
    "---      ",
    "         "
    ],
    [
    "         ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "---      ",
    "         "
    ],
    [
    "_______  ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "---      ",
    "         "
    ],
    [
    "_______  ",
    "|     |  ",
    "|     o  ",
    "|        ",
    "|        ",
    "|        ",
    "|        ",
    "---      ",
    "         "
    ],
    [
    "_______  ",
    "|     |  ",
    "|     o  ",
    "|    -+- ",
    "|        ",
    "|        ",
    "|        ",
    "---      ",
    "         "
    ],
    [
    "_______  ",
    "|     |  ",
    "|     o  ",
    "|    -+- ",
    "|     0  ",
    "|    / \\",
    "|        ",
    "---      ",
    "         "
    ]
  ]

main = do
  putStrLn "+-------------------------------------------+"
  putStrLn "|                Виселица                   |"
  putStrLn "+-------------------------------------------+"
  putStrLn ""
  putStrLn "[Хозяин таверны]: Смотрите, кто к нам пришел! Ну, присаживайся..."
  newRound

pick :: [a] -> IO (a, [a])
pick [] = error "Can't pick from empty list"
pick xs = do
  i <- randomRIO (0, (length xs) - 1)
  return ((xs !! i), (take (i) xs) ++ (drop (i+1) xs))

randomWord :: FilePath -> IO String
randomWord f = do
  content <- readFile f
  picked <- pick $ lines content
  return $ fst picked

newRound :: IO ()
newRound = do
  secretWord <- randomWord "dictionary.txt"
  putStrLn "[Хозяин таверны]: Итак! Новый раунд!"
  putStrLn ""
  putStrLn "================= НОВЫЙ РАУНД ==============="
  putStrLn ""
  putStrLn "[Хозяин таверны]: Я загадал слово! Отгадывай!"
  askForLetter secretWord [] 0

printCurrentState :: String -> [Char] -> Int -> IO()
printCurrentState secret letters currentState = do
  let answer = Prelude.map (\c -> if (elem c letters) then c else '_') secret
  putStrLn ""
  putStrLn "#################################"
  putStrLn "Текущее состояние игры: "
  putStrLn $ unlines (states !! currentState)
  putStrLn $ "СЛОВО: " ++ answer
  putStrLn "#################################"
  putStrLn ""


askForLetter :: String -> [Char] -> Int -> IO()
askForLetter secret letters currentState = do
  putStrLn "[Хозяин таверны]: Ну и какая же твоя следующая буква?"
  str <- getLine
  if (length str) > 1
    then do askForLetter secret letters currentState
    else do
      let c = (str !! 0)
      if (elem c letters)
        then do
          putStrLn "[Хозяин таверны]: Ты уже называл эту букву! Давай-ка подумай еще хорошенько!"
          askForLetter secret letters currentState
        else do
          if (elem c secret)
            then do
              putStrLn "[Хозяин таверны]: Отлично! Правильно!"
              printCurrentState secret (c:letters) currentState
              if (all (flip elem (c:letters)) secret)
                then do
                  putStrLn "[Хозяин таверны]: Ты выиграл! Мои поздравления! Можно сыграть ещё разок!"
                  newRound
                else do
                  askForLetter secret (c:letters) currentState
            else do
              if currentState == 4
                then do
                  printCurrentState secret (c:letters) (currentState + 1)
                  putStrLn "[Хозяин таверны]: Ох, как жаль, но ты проиграл, мой друг... Давай попробуем сыграть еще!"
                  newRound
                else do
                  putStrLn "[Хозяин таверны]: К сожалению этой буквы в слове нет... Ну, ничего! Все делают ошибки!"
                  printCurrentState secret (c:letters) (currentState + 1)
                  askForLetter secret (c:letters) (currentState + 1)











