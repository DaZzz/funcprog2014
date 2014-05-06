module TreeSet (Set, empty, insert, member, fromAscList) where

import AbstractSet
import qualified Data.List as L

data Tree t = EmptyTree | Node t (Tree t) (Tree t)
  deriving Show

singleton :: t -> Tree t
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord t) => t -> Tree t -> Tree t
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord t) => t -> Tree t -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

treeFromAscList :: Eq t => [t] -> Tree t
treeFromAscList l = foldl (\t x -> Node x EmptyTree t) EmptyTree (L.nub l)

treeMinValue :: Ord t => Tree t -> t
treeMinValue EmptyTree = error "No min value in epmty tree"
treeMinValue (Node a EmptyTree _) = a
treeMinValue (Node a l _) = treeMinValue l

treeRemove :: (Ord t) => t -> Tree t -> Tree t
treeRemove _ EmptyTree = EmptyTree
treeRemove x (Node a EmptyTree r) = r
treeRemove x (Node a l r)
  | x < a = Node a (treeRemove x l) r
  | x > a = Node a l (treeRemove x r)
  | x == a = Node (treeMinValue l) (treeRemove (treeMinValue l) l) r

newtype Set t = SetImpl (Tree t)

instance AbstractSet Set where
  empty = SetImpl EmptyTree

  insert x (SetImpl s) = SetImpl (treeInsert x s)

  member x (SetImpl s) = treeElem x s

  fromAscList l = SetImpl (treeFromAscList l)

  remove x (SetImpl s) = SetImpl (treeRemove x s)
