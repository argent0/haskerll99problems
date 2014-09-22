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
pack [x] = [[x]]
pack (x:xs) = (x:(takeWhile (== x) xs)):(pack (dropWhile (== x) xs))
