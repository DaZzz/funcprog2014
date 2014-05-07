module Task2 where

type Name = String
type Age = Int
type Group = String

data Student = Student Name Age Group
  deriving (Show)

listToTripletList :: [a] -> [(a,a,a)]
listToTripletList [] = []
listToTripletList (x1:x2:x3:xs) = (x1, x2, x3) : listToTripletList xs

parseString :: String -> [Student]
parseString s = map (\(n,a,g) -> Student n (read a :: Int) g) $ listToTripletList (lines s)

loadFromFile :: FilePath -> IO [Student]
loadFromFile f = readFile f >>= \content -> return $ parseString content

printStudents :: FilePath -> IO ()
printStudents f = loadFromFile f >>= \students -> print students


