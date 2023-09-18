module Ex5 where

-- we've seen classes and how we can implement methods
-- from them. Is there any particular method that is
-- useful to have sometimes?

-- Foldable
-- (many data structures work nicely with folds)
-- `foldl` can be expressed in terms of `foldr`
-- as `foldr` can work on infinite lists, while
-- `foldl` cannot.
-- FOLD <binary function> <accumulator> <data>
-- foldl f a bs = foldr (\b g x -> g(f x b)) id bs a

-- Requires:
-- 1. A container
-- 2. A binary operation f
-- 3. A starting value z

-- our old friend, (Binary) Tree
data Tree a
  = Empty
  | Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

-- a foldr function
treeFoldr f z Empty = z -- return the initial accum.
treeFoldr f z (Leaf x) = f x z -- apply f to the value of leaf
treeFoldr f z (Branch l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
  foldr = treeFoldr

-- Functor
-- has the `map` operation called `fmap`
-- Requires:
-- 1. A function from type A to type B (can be the same)
-- 2. A container of A
-- and returns a container of B

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- FUNCTOR LAWS
-- 1. fmap id = id (id a = a)
-- 2. fmap (f . g) = fmap f . fmap g (homomorphism)

-- Does it work for the above?
-- t = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
-- fmap id t
-- fmap ((+1) . (*2)) t
-- fmap (+1) $ fmap (*2) t