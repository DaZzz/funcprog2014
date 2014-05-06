module ListMap (Map, empty, insert, hasKey, getValue) where

import AbstractMap
import qualified Data.List as L

newtype Map k v = MapImpl [(k, v)]

instance AbstractMap Map where
  empty = MapImpl []

  insert (k, v) (MapImpl l) = MapImpl ((k, v) : filter (\a -> (fst a) /= k) l)

  hasKey k (MapImpl l) = any (\a -> fst a == k) l

  getValue k (MapImpl l) = (L.find (\a -> fst a == k) l) >>= (Just . snd)
