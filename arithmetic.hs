isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy 0 n = error "Division by 0"
isDivisibleBy m n = (==) (mod n m) 0

removeDivisibleBy :: Int -> [Int] -> [Int]
removeDivisibleBy 0 _ = error "Division by 0"
removeDivisibleBy m (n:ns) = filter (not . isDivisibleBy m) (n:ns)

--isPrime :: Int -> Bool
--isPrime 1 = True
--isPrime n = all (\x -> (not . isDivisibleBy x) n ) [2..(n-1)]

--takeNPrimes :: Int -> [Int]
--takeNPrimes 0 = []
--takeNPrimes n = nextPrime (takeNPrimes (n - 1))
--	where
--		nextPrime [] = [2]
--		nextPrime (p:ps) = (p:ps) ++ (take 1 ((foldr (.) id $ map (removeDivisibleBy) (p:ps)) [2..]))

-- prime number generator
primes = 2:(nextPrimes [2])
	where nextPrimes (p:ps) = newPrime:(nextPrimes (newPrime:p:ps))
		where newPrime = (head ((foldr (.) id $ map (removeDivisibleBy) (p:ps)) [p..])) 

isPrime :: Int -> Bool
isPrime n = (n > 1) && (n == head (dropWhile (<n) primes))



myGCD :: Int -> Int -> Int
myGCD a b	| a == b = a
				| otherwise = myGCD smallest (biggest - smallest)
				where
					smallest = min a b
					biggest = max a b	


coprime :: Int -> Int -> Bool
coprime a b = (myGCD a b) == 1

-- 5 Problem 34
-- (**) Calculate Euler's totient function phi(m).
--
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.
--
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1)
-- = 1.

totient :: Int -> Int
totient 1 = 1
totient n = length (filter (coprime n) [1..(n-1)])

-- 6 Problem 35
-- (**) Determine the prime factors of a given positive integer. Construct a
-- flat list containing the prime factors in ascending order.

primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = f:(primeFactors (div n f))
	where f =  head $ filter (\x -> (rem n x) == 0) $ takeWhile (<=n) primes
--	Problem 39 - prime range
--
primesR :: Int -> Int -> [Int]
primesR b t= takeWhile (<=t) $ dropWhile (<b) primes

-- 11 Problem 40
-- (**) Goldbach's conjecture.
--
-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system). Write a predicate to
-- find the two prime numbers that sum up to a given even integer.

goldbach :: Int -> (Int, Int)
goldbach n	| (n < 2) || (odd n) = error "Odd Numbers greater than 2"
				| otherwise = head [ (x,y) | x <- (primesR 2 n), y <- (primesR x n), (x + y) == n]

-- 12 Problem 41
-- (**) Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.
--
-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than say
-- 50. Try to find out how many such cases there are in the range 2..3000.

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even [a..b]
