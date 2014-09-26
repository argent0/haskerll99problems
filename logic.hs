import Data.List
-- 2 Problem 46
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result of
-- their respective operations; e.g. and(A,B) will succeed, if and only if both
-- A and B succeed.
--
-- A logical expression in two variables can then be written as in the following
-- example: and(or(A,B),nand(A,B)).
--
-- Now, write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool,Bool)]
table f = [ (x, y, f x y) | let b = [True, False], x <- b, y <- b]

-- 5 Problem 49
-- (**) Gray codes.
--
-- An n-bit Gray code is a sequence of n-bit strings constructed according to
-- certain rules. For example,
--
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

gray 1 = ["0", "1"]
gray n = map ('0':) (gray (n - 1)) ++ (map  ('1':) (reverse (gray (n - 1))))

data HuffmanTree = Root Int String | Trunk Int HuffmanTree HuffmanTree deriving Show

huffman :: [HuffmanTree] -> HuffmanTree
huffman [Root f c] =  (Root f c)
huffman [Root f c, Root ff cc]
	| f > ff = Trunk (f+ff) (Root f c) (Root ff cc)
	| otherwise = Trunk (f+ff) (Root ff cc) (Root f c)
huffman [Trunk f l r, Root ff ss]
	| f > ff = Trunk (f+ff) (Trunk f l r) (Root ff ss)
	| otherwise = Trunk (f+ff) (Root ff ss) (Trunk f l r)
huffman [Trunk f l r, Trunk ff ll rr]
	| f > ff = Trunk (f+ff) (Trunk f l r) (Trunk ff ll rr)
	| otherwise = Trunk (f+ff) (Trunk ff ll rr) (Trunk f l r)
huffman (s:ss) = huffman ((Trunk (add t tt) tt t):ts)
	where
		(t:tt:ts) = sortBy cmp (s:ss)
		cmp (Root x _) (Root y _) = compare x y
		cmp (Root x _) (Trunk y _ _) = compare x y
		cmp (Trunk x _ _) (Root y _) = compare x y
		cmp (Trunk x _ _) (Trunk y _ _) = compare x y
		add (Root x _) (Root y _) = (+) x y
		add (Root x _) (Trunk y _ _) = (+) x y
		add (Trunk x _ _) (Root y _) = (+) x y
		add (Trunk x _ _) (Trunk y _ _) = (+) x y

huffmanCode :: HuffmanTree -> [(String, String)]
huffmanCode (Root f c) = [("1", c)]
huffmanCode (Trunk _ (Root _ c)  (Root _ cc)) = [("1", c), ("0", cc)]
huffmanCode (Trunk _ t  (Root _ cc)) = (expandCode "1" $ huffmanCode t) ++ [("0", cc)]
	where expandCode c (d:ds) = (map (\x -> (c ++ (fst x), (snd x))) (d:ds))

huffmanCode (Trunk _ (Root _ c) t) = [("1", c)] ++ (expandCode "0" $ huffmanCode t)
	where expandCode c (d:ds) = (map (\x -> (c ++ (fst x), (snd x))) (d:ds))

huffmanCode (Trunk _ t tt) = (expandCode "1" $ huffmanCode t) ++ (expandCode "0" $ huffmanCode tt)
	where expandCode c (d:ds) = (map (\x -> (c ++ (fst x), (snd x))) (d:ds))

sfs = [
	(Root 45 "a" ),
	(Root 13 "b" ),
	(Root 12 "c" ),
	(Root 16 "d" ),
	(Root 9 "e" ),
	(Root 5 "f")] 

--Trunk 100
--
--1      (Trunk 55
--11      	(Trunk 30
--111-    		(Root 16 "d")
--110      		(Trunk 14
--1101-  			(Root 9 "e")
--1100-   			(Root 5 "f")))
--10      	(Trunk 25
--101-     		(Root 13 "b")
--100-     		(Root 12 "c")))
--0-     (Root 45 "a")
--
-- vim expandtab
