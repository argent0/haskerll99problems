module Main where
-- 5 Problem 57
-- (**) Binary search trees (dictionaries)
--
-- Use the predicate add/3, developed in chapter 4 of the course, to write a
-- predicate to construct a binary search tree from a list of integer numbers.
--
-- Example:
--
-- * construct([3,2,5,7,1],T).
-- T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
-- Then use this predicate to test the solution of the problem P56.
--
-- Example:
--
-- * test-symmetric([5,3,18,1,4,12,21]).
-- Yes
-- * test-symmetric([3,2,5,7,4]).
-- No
-- Example in Haskell:
--
-- *Main> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7
-- Empty Empty))
-- *Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- *Main> symmetric . construct $ [3, 2, 5, 7, 1]
-- True

import Data.Foldable

data Tree a = Empty | Leaf a | Branch a (Tree a) (Tree a) deriving Show

instance Foldable Tree  where
   foldr _ z Empty = z
   foldr f z (Leaf x) = f x z
   foldr f z (Branch k l r) = Data.Foldable.foldr f (f k (Data.Foldable.foldr f z r)) l

insert :: Ord a => a -> Tree a -> Tree a
insert n Empty = Leaf n
insert n (Leaf o)
   | n < o = Branch o (Leaf n) Empty
   | otherwise = Branch o Empty (Leaf n)
insert n (Branch o l r)
   | n < o = Branch o (insert n l) r
   | otherwise = Branch o l (insert n r)

insertList :: Ord a => [a] -> Tree a -> Tree a
insertList l t = Prelude.foldl (flip insert) t l

sameStructure :: Tree a -> Tree a -> Bool
sameStructure Empty Empty = True
sameStructure (Leaf _) (Leaf _) = True
sameStructure (Branch _ l r) (Branch _ ll rr) = sameStructure l ll && sameStructure r rr
sameStructure _ _ = False

mirrorImage :: Tree a -> Tree a
mirrorImage Empty = Empty
mirrorImage (Leaf a) = Leaf a
mirrorImage (Branch v l r) = Branch v (mirrorImage r) (mirrorImage l)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Leaf _) = True
symmetric (Branch _ l r) = sameStructure l (mirrorImage r)

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

-- 6 Problem 58
--
-- (**) Generate-and-test paradigm
--
-- Apply the generate-and-test paradigm to construct all symmetric, completely
-- balanced binary trees with a given number of nodes.
--
-- Example:
--
-- * sym-cbal-trees(5,Ts).
-- Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x,
-- t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
--
-- Example in Haskell:
--
-- *Main> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch
-- 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
-- Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

symCbalTrees :: a -> Int -> [Tree a]
symCbalTrees a = filter symmetric . flip cbbt a

-- 7 Problem 59
--
-- (**) Construct height-balanced binary trees
--
-- In a height-balanced binary tree, the following property holds for every
-- node: The height of its left subtree and the height of its right subtree are
-- almost equal, which means their difference is not greater than one.
--
-- Construct a list of all height-balanced binary trees with the given element
-- and the given maximum height.
--
-- Example:
--
-- ?- hbal_tree(3,T).
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil,
-- nil))) ;
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
-- etc......No
--
-- Example in Haskell:
--
-- *Main> take 4 $ hbalTree 'x' 3
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty
-- Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty)
--  Empty),
--   Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty)
--   (Branch 'x' Empty Empty)),
--    Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty
--    Empty)]
--
--    Solutions 

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree a 1 = [Leaf a]
hbalTree a n = 
   if even (n-1)
      then zipWith b (hbalTree a half) (hbalTree a half)
      else zipWith b (htree ++ htree) (hbalTree a (half+r)) ++ zipWith b (hbalTree a (half+r)) (htree ++ htree)
   where
   b = Branch a
   half = div (n-1) 2
   r = rem (n-1) 2
   htree = cbbt half a

main :: IO ()
main = putStrLn "Hello, World!"

-- vim: expandtab
