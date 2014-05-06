module DefaultSet (Set, empty, insert, member, fromAscList) where

import AbstractSet
import qualified Data.Set as S

newtype Set t = SetImpl (S.Set t)

instance AbstractSet Set where
  empty = SetImpl S.empty

  insert x (SetImpl s) = SetImpl (S.insert x s)

  member x (SetImpl s) = S.member x s

  fromAscList l = SetImpl (S.fromAscList l)

  remove x (SetImpl s) = SetImpl (S.delete x s)
