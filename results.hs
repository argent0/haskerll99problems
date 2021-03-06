import System.Random
-- 1
-- 3
elementAt (x:xs) y = (x:xs) !! (y-1)
--elementAt [1,2,3] 2 --2	

--4
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--5

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome $ init xs)

--7
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

-- 8
-- Eliminate consecutive duplicates of list elements

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
	| (x==next) = compress xs
	| otherwise = ( x:(compress xs))
	where next = xs !! 0

-- 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = (x:(takeWhile (== x) xs)):(pack (dropWhile (== x) xs))

-- 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.

encode :: (Eq a) => [a] -> [(Int, a)]
encode x = map (\ v -> (length v, v !! 0)) (pack x)

-- 11
-- (*) Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

data EncodeSymbol a = Single a | Multiple Int a deriving Show
encode_modified :: (Eq a) => [a] -> [EncodeSymbol a]
encode_modified x = map encoder (pack x)
	where
		encoder [x] = Single x
		encoder (x:xs) = Multiple (length (x:xs)) x
		

-- 12
-- (**) Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version
--
--Example in Haskell:
--
--P12> decodeModified 
--       [Multiple 4 'a',Single 'b',Multiple 2 'c',
--       Multiple 2 'a',Single 'd',Multiple 4 'e']
--"aaaabccaadeeee"

decodeModified :: [EncodeSymbol a] -> [a]
decodeModified x = foldr (++) [] (map decoder x)
	where
		decoder (Single a) = [a]
		decoder (Multiple n a) = replicate n a

--3 Problem 13
--(**) Run-length encoding of a list (direct solution).
--
--Implement the so-called run-length encoding data compression method directly.
--I.e. don't explicitly create the sublists containing the duplicates, as in
--problem 9, but only count them. As in problem P11, simplify the result list by
--replacing the singleton lists (1 X) by X.
--
--Example in Haskell:
--
--P13> encodeDirect "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect :: (Eq a) => [a] -> [EncodeSymbol a]
encodeDirect [] = []
encodeDirect (x:xs) 
	| (len==1) = (Single x):(encodeDirect xs)
	| otherwise = (Multiple len x):(encodeDirect $ dropWhile (==x) xs)
	where len = (+1) $ length $ takeWhile (==x) xs

-- 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli a = foldr (++) [] (map (replicate 2) a)

-- 15
-- (**) Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli a n = foldr (++) [] (map (replicate n) a)

--6 Problem 16
--(**) Drop every N'th element from a list.
--
--Example in Haskell:
--
-- Main> dropEvery "abcdefghik" 3
--"abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery (x:xs) n = (take (n - 1) (x:xs)) ++ (dropEvery (drop n (x:xs)) n)

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example in Haskell:
--
-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split [] n = ([], [])
split (x:xs) n =
	iter ([], n, (x:xs))
	where
		iter (acc, n, []) = (acc, [])
		iter (acc, n, (x:xs))
			| ((clength acc) < n) = iter ((acc ++ [x]), n, xs)
			| otherwise = (acc, (x:xs))
			where
				clength [] = 0
				clength (x:xs) = 1 + (clength xs)


--8 Problem 18
--(**) Extract a slice from a list.
--
--Given two indices, i and k, the slice is the list containing the elements
--between the i'th and k'th element of the original list (both limits included).
--Start counting the elements with 1.
--
--Example in Haskell:
--
--Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
--"cdefg"

slice :: [a] -> Int -> Int -> [a]
slice (x:xs) start end = takeN len $ dropN (start - 1) (x:xs)
	where
		len = end - start + 1
		dropN n [] = []
		dropN 0 (x:xs) = x:xs
		dropN n (x:xs) = dropN (n - 1) xs 
		takeN n [] = []
		takeN 0 (x:xs) = []
		takeN n (x:xs) = [x] ++ (takeN (n - 1) xs)

-- 9 Problem 19
-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
--
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
--
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--  
--  *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
--  "ghabcdef"
--

rotate :: [a] -> Int -> [a]
rotate (x:xs) n 
	| n >= 0 = dropN rotation (x:xs) ++ takeN rotation (x:xs)
	| otherwise = dropN (len - rotation) (x:xs) ++ takeN (len - rotation) (x:xs)
	where
		len = (length (x:xs))
		rotation = mod (abs n) len
		dropN n [] = []
		dropN 0 (x:xs) = x:xs
		dropN n (x:xs) = dropN (n - 1) xs 
		takeN n [] = []
		takeN 0 (x:xs) = []
		takeN n (x:xs) = [x] ++ (takeN (n - 1) xs)

-- Problem 20
-- (*) Remove the K'th element from a list.
--
-- Example in Prolog:
--
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- Example in Lisp:
--
-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also
-- returns the deleted element.)
--
-- Example in Haskell:
--
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n [] = (Nothing, [])
removeAt n (x:xs) =
		( return ((x:xs) !! (n - 1)) , ((init h)++ t))
		where
			(h,t) = (splitAt n (x:xs))

-- 1 Problem 21
-- Insert an element at a given position into a list.
--
-- Example:
--
-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:
--
-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt a (x:xs) n = (take (n - 1) (x:xs))  ++ (a:(drop (n - 1) (x:xs)))

-- 2 Problem 22
-- Create a list containing all integers within a given range.
--
-- Example:
--
-- * (range 4 9)
-- (4 5 6 7 8 9)
-- Example in Haskell:
--
-- Prelude> range 4 9
-- [4,5,6,7,8,9]
--
range :: Int -> Int -> [Int]
range start end
	| len == 0 = [start]
	| len > 0 = start:(range (start + 1) end)
	where len = end - start

-- 6 Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N
-- elements of a list
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We
-- all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
-- well-known binomial coefficients). For pure mathematicians, this result may
-- be great. But we want to really generate all the possibilities in a list.
--
-- Example:
--
-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
-- Example in Haskell:
--
-- > combinations 3 "abcdef"
-- ["abc","abd","abe",...]

-- Using ord constrain
--combinations :: (Ord a) => Int -> [a] -> [[a]]
--combinations n [] = []
--combinations 0 (x:xs) = []
--combinations 1 (x:xs) = map (:[]) (x:xs)
--combinations n (x:xs) = [x:y | x <- (x:xs), y <- (combinations (n - 1)  (filter (> x) xs))]

-- Using Eq constrain
-- requieres that elements don't repeat in the input list
-- combinations :: (Eq a) => Int -> [a] -> [[a]]
-- combinations n [] =	[]
-- combinations 0 (x:xs) =	[]
-- combinations 1 (x:xs) = map (:[]) (x:xs)
-- combinations n (x:xs) =	[h:y | h <- (x:xs), y <- (combinations (n-1) (coset h (x:xs)))]
-- 	where
-- 	coset h (x:xs) = remove (h:(takeWhile (/=h) (x:xs))) (x:xs)
-- 	remove list = foldl (.) (filter (\x -> True)) [filter (/=x) | x <- list]

-- Using Eq constrain
-- requieres that elements can be Eq but should not be next to each other
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations n [] =	[]
combinations 0 (x:xs) =	[]
combinations 1 (x:xs) = map (:[]) (x:xs)
combinations n (x:xs) =	[h:y | h <- (x:xs), y <- (combinations (n-1) (coset h (x:xs)))]
	where
	coset h (x:xs) = tail (dropWhile (/=h) (x:xs))

-- 7 Problem 27
-- Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
-- 2, 3 and 4 persons? Write a function that generates all the possibilities and
-- returns them in a list.
--
-- Example:
--
-- * (group3 '(aldo beat carla david evi flip gary hugo ida))
-- ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
-- ... )
-- b) Generalize the above predicate in a way that we can specify a list of
-- group sizes and the predicate will return a list of groups.
--
-- Example:
--
-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT)
-- ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference
-- between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
--
-- You may find more about this combinatorial problem in a good book on discrete
-- mathematics under the term "multinomial coefficients".
--
-- Example in Haskell:
--
-- P27> group [2,3,4]
-- ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)
--  
--  27> group [2,2,5]
--  ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--  [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
--  (altogether 756 solutions)
--

group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group [] x = []
group (g:gs) (i:is)
	| is_posible =  distribute (g:gs) (i:is)
	| otherwise = error "Es ist nich mochlich"
	where
	is_posible = (<=) (sum (g:gs)) (length (i:is))
	sum (x:xs) = foldr (+) 0 (x:xs)
	distribute [g] (i:is) = map (:[]) (combinations g (i:is))
	distribute (g:gs) (i:is) = [[x] ++ y | x <- (combinations g (i:is)), y <- (distribute gs (filter (\ e -> not (elem e x)) (i:is))) ] --map (:[]) (combinations g (i:is))
	solve_element k (1:gs) (i:is) = map ([[k]]++) (distribute gs (remove_item k (i:is)))
	remove_item k (x:xs) = (takeWhile (/= k) (x:xs)) ++ (tail (dropWhile (/= k) (x:xs)))

-- 8 Problem 28
-- Sorting a list of lists according to length of sublists
--
-- a) We suppose that a list contains elements that are lists themselves. The
-- objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.
--
-- Example:
--
-- * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
-- Example in Haskell:
--
-- Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude>["o","de","de","mn","abc","fgh","ijkl"]
-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to
-- their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more
-- frequent length come later.
--
-- Example:
--
-- * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
-- Example in Haskell:
--
-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (l:ls) = lsort([n | n <- ls, (cmp n l == LT)])++ [l] ++ lsort([m | m <- ls , (cmp m l /= LT)])
	where cmp a b = compare (length a) (length b)

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort (l:ls) = lfsort([n | n <- ls, (cmp n l == LT)])++ [l] ++ lfsort([m | m <- ls , (cmp m l /= LT)]) 
	where
      cmp a b = compare (length (filter_lenght (length a) (l:ls))) (length (filter_lenght (length b) (l:ls)))
      filter_lenght l = filter (\ x -> ((==) l (length x)))

-- vim: expandtab
