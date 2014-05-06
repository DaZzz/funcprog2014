module ListGraph (Show, Graph, mkGraph, edgeIn, adjacent, nodes, weight, bounds, edges, fromGraph) where

import AbstractGraph
import Data.Array
import Data.List
import Data.Maybe


newtype Graph n w = GraphImpl (Array n [(n, w)])

instance (Show a, Show b, Ix a) => Show (Graph a b) where
  show (GraphImpl a) = show a

instance AbstractGraph Graph where

  mkGraph directed bounds edges = GraphImpl $ accumArray (flip (:)) [] bounds edges'
    where
      edges' = if directed
        then edges''
        else edges'''

      edges'' = map (\(n1, n2, w) -> (n1, (n2, w))) edges
      edges''' = (map (\(n1, n2, w) -> (n2, (n1, w))) edges) ++ (edges'')

  edgeIn (GraphImpl g) (v1, v2) = (elem v2 fromV1)
    where
      fromV1 = map fst $ g ! v1

  adjacent gr@(GraphImpl g) v = fromV ++ toV
    where
        fromV = map fst (g ! v)
        toV = filter (\v1 -> edgeIn gr (v1, v)) $ range (bounds g)

  nodes (GraphImpl graph) = range (bounds graph)

  nodesBounds (GraphImpl graph) = bounds graph

  weight v1 v2 gr@(GraphImpl g) =
    if isNothing weight
      then error "No such edge in this graph."
      else snd $ fromJust weight
    where
      weight = find (\(v, w) -> v2 == v) (g ! v1)

  fromGraph g = mkGraph True (nodesBounds g) (edges g)

  edges = undefined



