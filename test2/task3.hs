import System.Environment
import Data.List

-- Types
type Record = (String, String, String, String, [Int])
type DataBase = [Record]

-- Main function
main = do
  [cmd, f] <- getArgs
  case cmd of
    "-s" -> staff f
    "-n" -> projectNumbers f
    "-t" -> topProject f

-- Staff
parseProjects :: [String] -> [Int]
parseProjects s = map (\p -> read p :: Int) s

recordFromString :: String -> Record
recordFromString s = (\(c:sn:n:mn:projects) -> (c,sn,n,mn, (parseProjects projects))) (words s)

loadDataBase :: FilePath -> IO DataBase
loadDataBase f = do
  content <- readFile f
  return $ map recordFromString $ lines content

allStaffNames :: DataBase -> [String]
allStaffNames db = map (\(_,sn,n,mn,_) -> sn ++ " " ++ n ++ " " ++ mn) db

staff :: FilePath -> IO()
staff f = do
  db <- loadDataBase f
  let names = allStaffNames db
  putStrLn "List of staff:"
  putStrLn $ unlines (sort names)

-- Project numbers
allProjectNumbers :: DataBase -> [Int]
allProjectNumbers db = nub $ concat $ map (\(_,_,_,_,projects) -> projects) db

projectNumbers :: FilePath -> IO()
projectNumbers f = do
  db <- loadDataBase f
  putStrLn "List of projects:"
  putStrLn $ intercalate ", " $ map show $ allProjectNumbers db

-- Top projects and names
getProjects :: Record -> [Int]
getProjects (_,_,_,_,ps) = ps

getTopProject :: DataBase -> Int
getTopProject db = snd $ head $ reverse $ sort $ map (\g -> (length g, head g)) $ group $ sort $ concat $ map getProjects db

getStaffForProject :: DataBase -> Int -> DataBase
getStaffForProject db p = filter (\r -> elem p (getProjects r)) db

topProject :: FilePath -> IO()
topProject f = do
  db <- loadDataBase f

  let tp = getTopProject db
  putStrLn "Top project: "
  print $ getTopProject db

  let db' = getStaffForProject db tp
  let names = allStaffNames db'

  putStrLn "Staff: "
  putStrLn $ unlines (sort names)























