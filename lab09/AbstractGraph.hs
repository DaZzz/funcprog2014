module AbstractGraph (AbstractGraph, mkGraph, edgeIn, adjacent, nodes, weight, nodesBounds, edges, fromGraph) where

import Data.Ix

class AbstractGraph g where
  mkGraph :: (Ix n, Num w) => Bool -> (n, n) -> [(n, n, w)] -> g n w

  edgeIn :: (Ix n, Num w) => g n w -> (n, n) -> Bool

  adjacent :: (Ix n, Num w) => g n w -> n -> [n]

  nodes :: (Eq w, Ix n, Num w) => g n w -> [n]

  weight :: (Ix n, Num w) => n -> n -> g n w -> w

  fromGraph :: (Ix n, Num w) => g n w -> g n w

  nodesBounds :: (Ix n, Num w) => g n w -> (n, n)

  edges :: (Ix n, Num w) => g n w -> [(n,n,w)]