module ListSet (Set, empty, insert, member, fromAscList) where

import AbstractSet
import qualified Data.List as L

newtype Set t = SetImpl [t]

instance AbstractSet Set where
  empty = SetImpl []

  insert x (SetImpl s) = if elem x s then SetImpl s else SetImpl (x:s)

  member x (SetImpl s) = elem x s

  fromAscList l = SetImpl (L.nub l)

  remove x (SetImpl s) = SetImpl (L.delete x s)
