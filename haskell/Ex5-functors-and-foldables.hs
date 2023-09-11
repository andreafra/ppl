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

-- our old friend
data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

-- a foldr function
treeFoldr f z Empty = z -- return the initial accum.
treeFoldr f z (Leaf x) = f x z -- apply f to the value of leaf
treeFoldr f z (Node l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
    foldr = treeFoldr

-- Functor
-- has the `map` operation called `fmap`

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f Leaf a = 