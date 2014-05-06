module DefaultMap (Map, empty, insert, hasKey, getValue) where

import AbstractMap
import qualified Data.Map as M

newtype Map k v = MapImpl (M.Map k v)

instance AbstractMap Map where
  empty = MapImpl M.empty

  insert (k, v) (MapImpl m) = MapImpl (M.insert k v m)

  hasKey k (MapImpl m) = M.member k m

  getValue k (MapImpl m) = M.lookup k m
