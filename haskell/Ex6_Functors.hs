module Ex6_Functors where

-- let's say that we want to avoid crashing when doing certain operations
-- such as division by 0.

goodResult = 4 `div` 2 -- 2

badResult = 4 `div` 0 -- Exception -> Crash

-- let's say we want to make a 'safeDiv' that returns a 'Result' type
-- that when everything is good returns 'Ok' containing the result
-- and when it goes wrong it return an 'Err'

data Result a = Err | Ok a
  deriving (Eq, Ord, Show)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok (n `div` m)

-- now we want to be able to apply other functions to the result
-- but we don't want to unwrap it
-- if the result is 'Ok', we apply a function and update its value
-- if the result is 'Err', we don't do anything and keep 'Err'
-- HOW? using a FUNCTOR

instance Functor Result where
  fmap f (Ok a) = Ok $ f a
  fmap _ Err = Err

-- Functor
-- has the `map` operation called `fmap`
-- Requires:
-- 1. A function from type A to type B (can be the same)
-- 2. A container of A
-- and returns a container of B
-- In a nutshell, a FUNCTOR applies a FUNCTION to ITS (the functor's) content

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- we can call fmap with an infix notation as "f <$> t"

-- FUNCTOR LAWS
-- 1. fmap id = id (where id a = a)
-- 2. fmap (f . g) = fmap f . fmap g (homomorphism)

-- Does it work for the above?
-- t = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
-- fmap id t
-- fmap ((+1) . (*2)) t
-- fmap (+1) $ fmap (*2) t