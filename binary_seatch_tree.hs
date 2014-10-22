module Trees where
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

data Tree a = Empty | Leaf a | Branch a (Tree a) (Tree a)

instance Foldable Tree  where
   foldr _ z Empty = z
   foldr f z (Leaf x) = f x z
   foldr f z (Branch k l r) = Data.Foldable.foldr f (f k (Data.Foldable.foldr f z r)) l

instance Show a => Show (Tree a) where
   show t = "\n" ++ go 0 t
      where
      go pad Empty = replicate pad ' ' ++ "Empty"
      go pad (Leaf v) = replicate pad ' ' ++ "Leaf  " ++ show v
      go pad (Branch v l r) =
         replicate pad ' ' ++ "Branch  " ++ show v ++ "\n" ++
         replicate pad ' ' ++ " " ++ go (pad + 1) l ++ "\n" ++
         replicate pad ' ' ++ " " ++ go (pad + 1) r -- ++ "\n"

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

--cbbt :: a -> Int -> [Tree a]
--cbbt _ 0 = [Empty]
--cbbt a 1 = [Leaf a]
--cbbt a n
--   | n < 0 = undefined
--   | otherwise  =
--      if even (n-1)
--         then zipWith b (cbbt a half) (cbbt a half)
--         else zipWith b (dpl htree) (cbbt a (half+r)) ++ zipWith b (cbbt a (half+r)) (dpl htree)
--      where
--      b = Branch a
--      half = div (n-1) 2
--      r = rem (n-1) 2
--      htree = cbbt a half
--      dpl [] = []
--      dpl (x:xs) = x : x : dpl xs

cbbt' :: a -> Int -> [Tree a]
cbbt' _ 0 = [Empty]
cbbt' a 1 = [Leaf a]
cbbt' a n
   | n < 0 = undefined
   | even n = original_even -- ++ map mirrorImage (filter (not . symmetric) original_even)
   | otherwise = original_odd -- ++ map mirrorImage (filter (not . symmetric) original_odd)
   where
   original_even = [b x y | x <-cbbt' a (half+1), y<-cbbt' a half] ++
      [b y x | x <-cbbt' a (half+1), y<-cbbt' a half]
   original_odd = [b x y | x <-cbbt' a half, y<-cbbt' a half]
   half = (n-1) `div` 2
   b Empty Empty = Leaf a
   b l r = Branch a l r


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
symCbalTrees a = filter symmetric . cbbt' a

{-

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
-- [Branch 'x'
--    (Branch 'x' Empty Empty)
--    (Branch 'x' Empty 
--                (Branch 'x' Empty Empty)),
-- Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty)
-- Empty),
-- Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty)
-- (Branch 'x' Empty Empty)),
-- Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty
-- Empty)]
--
--    Solutions 

toLeaf :: Tree a -> Tree a
toLeaf (Branch x Empty Empty) = Leaf x
toLeaf x = x

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
--hbalTree a 1 = [Leaf a, Empty]
hbalTree a h 
   | h < 0 = [Empty]
   | otherwise = filter isHbalTree $ map toLeaf $
   Prelude.concatMap (\x ->
      zipWith b (hbalTree a x) (hbalTree a x))
      [h-1] ++
   Prelude.concatMap (\x->
      zipWith b (hbalTree a x) (hbalTree a (x-1)))
      [h-1] ++
   Prelude.concatMap (\x->
      zipWith b (hbalTree a x) (hbalTree a (x+1)))
      [0..(h-2)]
   where
   b = Branch a

-- x - y =1 => y = x - 1
-- x - y = (-1) => y = x + 1


--hbalTree a n = 
--   if even (n-1)
--      then zipWith b (hbalTree a half) (hbalTree a half)
--      else zipWith b (htree ++ htree) (hbalTree a (half+r)) ++ zipWith b (hbalTree a (half+r)) (htree ++ htree)
--   where
--   b = Branch a
--   half = div (n-1) 2
--   r = rem (n-1) 2
--   htree = cbbt half a

height :: Tree a -> Int
height Empty = 0
height (Leaf _) = 1
height (Branch _ l r) = 1 + max (height l) (height r)

isHbalTree :: Tree a -> Bool
isHbalTree Empty = True
isHbalTree (Leaf _) = True
isHbalTree (Branch _ l r) = isHbalTree l && isHbalTree r && (abs (height l - height r) <= 1)

-}

-- vim: expandtab
