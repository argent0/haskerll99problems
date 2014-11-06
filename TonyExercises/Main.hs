{-# LANGUAGE TupleSections #-}
module Main where

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ [] = []
  furry f (a:as) = f a : furry f as

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b) deriving Show
newtype EitherRight a b = EitherRight (Either a b) deriving Show

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft (Left (f x))
  furry _ (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Right x)) = EitherRight (Right (f x))
  furry _ (EitherRight (Left x)) = EitherRight (Left x)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f) -- (>>=) (return . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana _ [] = []
  banana f (a:as) = f a ++ banana f as
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana f (Just x) = f x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
--banana :: (a -> ((->) t b) -> ((->) t a) -> ((->) t b)
  banana f g x = f (g x) x
  unicorn x _ = x  -- unicorn x = \_ -> x

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
--banana :: (a->(EitherLeft t b)) -> (EitherLeft t a) -> (EitherLeft t b)
  banana _ (EitherLeft (Right x)) = EitherLeft . Right $ x
  banana f (EitherLeft (Left x)) = f x
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left x)) = EitherRight . Left $ x
  banana f (EitherRight (Right x)) = f x
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
-- join
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id -- (>>=) id

-- Exercise 13
-- Relative Difficulty: 6
-- ap
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x = banana (`furry'` x)  --(>>=) (`fmap` x)

-- Exercise 14
-- Relative Difficulty: 6
-- forM
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (a:as) f =	banana 
						(\x -> banana (\xx -> unicorn (x:xx)) (moppy as f) ) $
						f a

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
-- sequence
sausage :: (Misty m) => [m a] -> m [a]
sausage = (`moppy` id) --(`forM` id)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f x y = apple y (furry' f x) -- y <*> f <$> x

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f x y z = apple z (banana2 f x y) -- z <*> y <*> f <$> x

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f x y z zz = apple zz (banana3 f x y z)

newtype State s a = State {
  state :: s -> (s, a)
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
--furry :: (a -> b) -> State s a -> State s b
  furry f (State g) = State (\x -> (fst (g x), f (snd (g x))))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
--banana :: (a -> (State s b)) -> (State s a) -> (State s b)
--g :: s -> (s,a)
  banana f (State g) = State (\x -> (fst (g x), snd (state (f (snd (g x))) x) ))
  unicorn x = State (,x)

main :: IO ()
main = putStrLn "Hello, World!"

-- vim: expandtab
