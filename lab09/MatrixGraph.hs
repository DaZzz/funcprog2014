module MatrixGraph (Show, Graph, mkGraph, edgeIn, adjacent, nodes, weight, bounds, edges, fromGraph) where

import AbstractGraph
import Data.Array
import Data.List
import Data.Maybe

newtype Graph n w = GraphImpl (Array n (Array n (Maybe w)))

instance (Show a, Show b, Ix a) => Show (Graph a b) where
  show (GraphImpl a) = show a

instance AbstractGraph Graph where

  mkGraph directed bounds edges = GraphImpl $ listArray bounds rows
    where
      rows = [ listArray bounds (initRow i) | i <- range bounds]
      initRow i = [ initElement i j | j <- range bounds]
      initElement i j = (find (edgeExists i j) edges) >>= maybeThird
      edgeExists i j (n1, n2, w) = if directed
        then n1 == i && n2 == j
        else (n1 == i && n2 == j) || (n2 == i && n1 == j)
      maybeThird = (\(_,_,a) -> Just a)

  edgeIn (GraphImpl g) (v1, v2) = isJust (g ! v1 ! v2)

  adjacent gr@(GraphImpl g) v = fromV ++ toV
    where
      fromV = map fst $ filter (\(_, w) -> isJust w) $ assocs (g ! v)
      toV = map fst $ filter (\(_, w) -> isJust w) $ column
      column = map (\(v', row) -> (v', row ! v)) $ assocs g

  nodes (GraphImpl graph) = range (bounds graph)

  weight v1 v2 gr@(GraphImpl g) =
    if isNothing (g ! v1 ! v2)
      then error "No such edge in this graph."
      else fromJust (g ! v1 ! v2)

  fromGraph g = mkGraph True (nodesBounds g) (edges g)

  nodesBounds (GraphImpl graph) = bounds graph

  edges = undefined

