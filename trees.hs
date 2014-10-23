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
hbalTree a 1 = [Leaf a]
hbalTree a h
   | h < 0 = undefined
   | otherwise =  [b x y | x <-hbalTree a (h-1), y<-hbalTree a (h-2)] ++
      [b y x | x <-hbalTree a (h-1), y<-hbalTree a (h-2)] ++
      [b y x | x <-hbalTree a (h-1), y<-hbalTree a (h-1)]
   where
   b Empty Empty = Leaf a
   b l r = Branch a l r


--hbalTree a h 
--   | h < 0 = undefined
--   | even h = original_even
--   | otherwise = original_odd
--   where
--   original_even = [b x y | x <-hbalTree a (half+1), y<-hbalTree a half] ++
--      [b y x | x <-hbalTree a (half+1), y<-hbalTree a half]
--   original_odd = [b x y | x <-hbalTree a half, y<-hbalTree a half]
--   b Empty Empty = Leaf a
--   b l r = Branch a l r
--   half = (h-1) `div` 2

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

-- Problrm 60
-- (**) Construct height-balanced binary trees with a given number of nodes
--
-- Consider a height-balanced binary tree of height H. What is the maximum number
-- of nodes it can contain?
-- Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This
-- question is more difficult. Try to find a recursive statement and turn it into
-- a function
-- minNodes
-- that returns the minimum number of nodes in a height-balanced binary tree of
-- height H. On the other hand, we might ask: what is the maximum height H a
-- height-balanced binary tree with N nodes can have? Write a function
-- maxHeight
-- that computes this.
--
-- Now, we can attack the main problem: construct all the height-balanced binary
-- trees with a given number of nodes. Find out how many height-balanced trees
-- exist for N = 15.
--
-- Example in Prolog:
-- 
--   ?- count_hbal_trees(15,C).
-- C = 1553
--
-- Example in Haskell:
--
--  *Main> length $ hbalTreeNodes 'x' 15
--  1553
--  *Main> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
--  [Branch 'x' Empty Empty],
--   [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty
--   Empty) Empty],
--    [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

-- The minimum number of nodes for a tree of height h
minNodes :: Int -> Int
minNodes = (!!) $ map minNodes' [0..]
   where
      minNodes' 0 = 0
      minNodes' 1 = 1
      minNodes' h = 1 + minNodes (h - 1) + minNodes (h - 2)

-- The max height for a tree wih n nodes.
maxHeight :: Int -> Int
maxHeight = (!!) $ map maxHeight' [0..]
   where
      maxHeight' n = head (filter (\x ->  minNodes x >=n) [0..n]) - 1
      --1 + maxHeight (half+r)
         --where
         --half = (n-1) `div` 2
         --r = (n-1) `rem` 2

nodes :: Tree a -> Int
nodes = Data.Foldable.foldr (\_ b -> b+1) 0

hbalTrees :: k -> Int -> [Tree k]
hbalTrees _ 0 = [Empty]
hbalTrees k n =
   filter (\x -> nodes x == n) $ Data.Foldable.concat $ map (hbalTree k) [0..mh]
   where
   mh = maxHeight n

-- vim: expandtab
