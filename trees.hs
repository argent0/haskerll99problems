module Trees where

import Data.Foldable

data Tree a = Empty | Leaf a | Branch a (Tree a) (Tree a)

instance Foldable Tree  where
   foldr _ z Empty = z
   foldr f z (Leaf x) = f x z
   foldr f z (Branch k l r) = Data.Foldable.foldr f (f k (Data.Foldable.foldr f z r)) l

instance Functor Tree where
   fmap _ Empty = Empty
   fmap f (Leaf x) = Leaf (f x)
   fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)

instance Show a => Show (Tree a) where
   show t = "\n" ++ go 0 t
      where
      go pad Empty = replicate pad ' ' ++ "Empty"
      go pad (Leaf v) = replicate pad ' ' ++ "Leaf  " ++ show v
      go pad (Branch v l r) =
         replicate pad ' ' ++ "Branch  " ++ show v ++ "\n" ++
         replicate pad ' ' ++ " " ++ go (pad + 1) l ++ "\n" ++
         replicate pad ' ' ++ " " ++ go (pad + 1) r -- ++ "\n"

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

--  2 Problem 61
--
--  Count the leaves of a binary tree
--
--  A leaf is a node with no successors. Write a predicate count_leaves/2 to
--  count them.
--
--  Example:
--
--  % count_leaves(T,N) :- the binary tree T has N leaves
--
--  Example in Haskell:
--
--  > countLeaves tree4
--  2

countLeaves :: Ord k => Tree k -> Int
countLeaves Empty = 0
countLeaves (Leaf _) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

--  3 Problem 61A
--
--  Collect the leaves of a binary tree in a list
--
--  A leaf is a node with no successors. Write a predicate leaves/2 to collect
--  them in a list.
--
--  Example:
--
--  % leaves(T,S) :- S is the list of all leaves of the binary tree T
--
--  Example in Haskell:
--
--  > leaves tree4
--  [4,2]

leaves :: Ord k => Tree k -> [k]
leaves Empty = []
leaves (Leaf x) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

--  4 Problem 62
--
--  Collect the internal nodes of a binary tree in a list
--
--  An internal node of a binary tree has either one or two non-empty
--  successors. Write a predicate internals/2 to collect them in a list.
--
--  Example:
--
--  % internals(T,S) :- S is the list of internal nodes of the binary tree T.
--
--  Example in Haskell:
--
--  Prelude>internals tree4
--  Prelude>[1,2]

internals :: Ord k => Tree k -> [k]
internals Empty = []
internals (Leaf _) = []
internals (Branch x l r) = internals l ++ [x] ++ internals r

--  5 Problem 62B
--
--  Collect the nodes at a given level in a list
--
--  A node of a binary tree is at level N if the path from the root to the node
--  has length N-1. The root node is at level 1. Write a predicate atlevel/3 to
--  collect all nodes at a given level in a list.
--
--  Example:
--
--  % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
--
--  Example in Haskell:
--
--  Prelude>atLevel tree4 2
--  Prelude>[2,2]

atLevel :: Ord k => Tree k -> Int -> [k]
atLevel = go
   where
   go _ 0 = []
   go Empty _ = []
   go (Leaf x) 1 = [x]
   go (Leaf _) _ = []
   go (Branch x _ _) 1 = [x]
   go (Branch _ l r) lv = go l (lv-1) ++ go r (lv - 1)

--  6 Problem 63
--
--  Construct a complete binary tree
--
--  A complete binary tree with height H is defined as follows:
--
--  The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e
--  2**(i-1) at the level i)
--  In level H, which may contain less than the maximum possible number
--  of nodes, all the nodes are "left-adjusted". This means that in a
--  levelorder tree traversal all internal nodes come first, the leaves
--  come second, and empty successors (the nil's which are not really
--  nodes!) come last. 
--
--  Particularly, complete binary trees are used as data structures (or
--  addressing schemes) for heaps.
--
--  We can assign an address number to each node in a complete binary
--  tree by enumerating the nodes in level-order, starting at the root
--  with number 1. For every node X with address A the following
--  property holds: The address of X's left and right successors are 2*A
--  and 2*A+1, respectively, if they exist. This fact can be used to
--  elegantly construct a complete binary tree structure.
--
--  Write a predicate complete_binary_tree/2.
--
--  Example:
--
--  % complete_binary_tree(N,T) :- T is a complete binary tree with N
--  nodes.
--
--  Example in Haskell:
--
--  Main> completeBinaryTree 4
--  Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x'
--  Empty Empty)
--   
--   Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty)
--   (Branch 'x' Empty Empty)
--   True

completeBinaryTree :: Ord k => k -> Int -> Tree k
completeBinaryTree _ 0 = Empty
completeBinaryTree k 1 = Leaf k
completeBinaryTree k n = Branch k (completeBinaryTree k (half + r)) (completeBinaryTree k half)
   where
   half = (n-1) `div` 2
   r = rem (n-1) 2
--  7 Problem 64
--
--  Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a
--  preparation for drawing the tree, a layout algorithm is required to
--  determine the position of each node in a rectangular grid. Several layout
--  methods are conceivable, one of them is shown in the illustration below:
--
--  p64.gif
--
--  In this layout strategy, the position of a node v is obtained by the
--  following two rules:
--
--  x(v) is equal to the position of the node v in the inorder sequence
--  y(v) is equal to the depth of the node v in the tree 
--
--  Write a function to annotate each node of the tree with a position,
--  where (1,1) in the top left corner or the rectangle bounding the
--  drawn tree.
--
--  Example in Haskell:
--
--  > layout tree64
--  Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

layout' :: Ord k => Tree k -> Tree (k,(Int,Int))
layout' = go 0 0
   where
   go _ _ Empty = Empty
   go ox oy (Leaf x) = Leaf (x,(ox,oy))
   go ox oy (Branch x l r) = Branch (x,(ox,oy)) nl nr
      where
      nl = go (ox-1) (oy+1) l
      nr = go (ox+1) (oy+1) r

layout :: Ord k => Tree k -> Tree (k,(Int,Int))
layout t = fmap (translate minX) lo
   where
   lo = layout' t
   minX = Data.Foldable.minimum $ fmap (\(_,(x,_)) -> x) lo
   translate d (k, (x,y)) = (k, (x-d,y))

-- vim: expandtab
