module AbstractSet where

class AbstractSet a where
  empty :: a t
  insert :: Ord t => t -> a t -> a t
  member :: Ord t => t -> a t -> Bool
  fromAscList :: Eq t => [t] -> a t
  remove :: (Eq t, Ord t) => t -> a t -> a t