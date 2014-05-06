import Criterion.Main

import AbstractGraph
import qualified ListGraph as LG
import qualified MatrixGraph as MG

-- Functions
loadFromFile :: (AbstractGraph g) => FilePath -> IO (g Int Int)
loadFromFile f = do
  content <- readFile f
  let (paramsStr : edgesStr) = lines content
  let toTuple3 (a:b:c:_) = (read a, read b, read c)
  let edges = map (\e -> toTuple3 (words e)) edgesStr
  let [vertex_count, directed] = words paramsStr
  return $ mkGraph (read directed :: Bool) (1, read vertex_count :: Int) edges

-- Tests
testMkGrpaphLG n = head $ nodes $ ((mkGraph True (1, n) (zip3 [1..n] (reverse [1..n]) [1..n])) :: LG.Graph Int Int)

testMkGrpaphMG n = head $ nodes $ ((mkGraph True (1, n) (zip3 [1..n] (reverse [1..n]) [1..n])) :: MG.Graph Int Int)

testGetAdjacent g n = foldl (\_ v -> head (adjacent g v)) 0 [1..n]

testGetNodes g = sum $ nodes g

-- Main
main = do
  let g1 = (mkGraph True (1, 20) [(x,y,z) | x <- [1..20], y <- [1..20], z <- [1..20]] :: LG.Graph Int Int)
  let g2 = (mkGraph True (1, 20) [(x,y,z) | x <- [1..20], y <- [1..20], z <- [1..20]] :: MG.Graph Int Int)

  defaultMain [
    bgroup "ListGraph" [
      bench "mkGraph 100" $ nf testMkGrpaphLG 100
      , bench "adjacent 20" $ nf (testGetAdjacent g1) 20
      , bench "nodes 20" $ nf testGetNodes g1
      ]
    , bgroup "MatrixGraph" [
      bench "mkGraph 100" $ nf testMkGrpaphMG 100
      , bench "adjacent 20" $ nf (testGetAdjacent g2) 20
      , bench "nodes 20" $ nf testGetNodes g2
      ]
    ]


