module AbstractMap where

class AbstractMap a where
  empty :: a k v
  insert :: (Eq k, Ord k) => (k, v) -> a k v -> a k v
  hasKey :: (Eq k, Ord k) => k -> a k v -> Bool
  getValue :: (Eq k, Ord k) => k -> a k v -> Maybe v
