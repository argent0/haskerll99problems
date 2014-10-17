module Trees where

import Data.Foldable

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

instance Foldable Tree  where
   foldr _ z Empty = z
   foldr f z (Branch x Empty Empty) = f x z
   foldr f z (Branch k l r) = Data.Foldable.foldr f (f k (Data.Foldable.foldr f z r)) l

leaf :: a -> Tree a
leaf a = Branch a Empty Empty

-- 3 Problem 55
--
-- (**) Construct completely balanced binary trees
--
-- In a completely balanced binary tree, the following property holds for every
-- node: The number of nodes in its left subtree and the number of nodes in its
-- right subtree are almost equal, which means their difference is not greater
-- than one.
--
-- Write a function cbal-tree to construct completely balanced binary trees for
-- a given number of nodes. The predicate should generate all solutions via
-- backtracking. Put the letter 'x' as information into all nodes of the tree.
--
--
cbbt :: Int -> a -> [Tree a]
cbbt 0 _ = [Empty]
cbbt n a
   | n < 0 = undefined
   | otherwise  =
      if even (n-1)
         then zipWith b (cbbt half a) (cbbt half a)
         else zipWith b (htree ++ htree) (cbbt (half+r) a) ++ zipWith b (cbbt (half+r) a) (htree ++ htree)
      where
      b = Branch a
      half = div (n-1) 2
      r = rem (n-1) 2
      htree = cbbt half a

isCbbt :: Tree a -> Bool
isCbbt Empty = True
isCbbt (Branch _ Empty Empty) = True
isCbbt (Branch _ l r) = almosEqual && isCbbt l && isCbbt r
   where
   almosEqual = abs(ll-lr) <= 1
   ll = length (toList l)
   lr = length (toList r)

--  4 Problem 56
--
--  (**) Symmetric binary trees
--
--  Let us call a binary tree symmetric if you can draw a vertical line through
--  the root node and then the right subtree is the mirror image of the left
--  subtree. Write a predicate symmetric/1 to check whether a given binary tree
--  is symmetric. Hint: Write a predicate mirror/2 first to check whether one
--  tree is the mirror image of another. We are only interested in the
--  structure, not in the contents of the nodes.
--
--  Example in Haskell:
--
--  *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
--  False
--  *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty
--  Empty))
--  True

sameStructure :: Tree a -> Tree a -> Bool
sameStructure Empty Empty = True
sameStructure Empty _ = False
sameStructure _ Empty = False
sameStructure (Branch _ Empty Empty) (Branch _ Empty Empty) = True
sameStructure (Branch _ Empty _) (Branch _ _ Empty) = False
sameStructure (Branch _ _ Empty) (Branch _ Empty _) = False
sameStructure (Branch _ Empty r) (Branch _ Empty rr) = sameStructure r rr
sameStructure (Branch _ l Empty) (Branch _ ll Empty) = sameStructure l ll
sameStructure (Branch _ l r) (Branch _ ll rr) = sameStructure l ll && sameStructure r rr

mirrorImage :: Tree a -> Tree a
mirrorImage Empty = Empty
mirrorImage b@(Branch _ Empty Empty) = b
mirrorImage (Branch x l r) = Branch x (mirrorImage r) (mirrorImage l)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = sameStructure l (mirrorImage r)


-- vim: expandtab
