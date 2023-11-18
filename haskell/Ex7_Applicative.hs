module Ex7 where

import Ex6_Functors (Result)

-- if we do:
-- :t (+) <$> Ok 3
-- we get a function and a value both wrapped into Ok (Result), but with no way to apply
-- one to the other.

-- For now, we don't really have an easy way to sum the values inside two Okes
-- (<$> Ok 3) <$> ((+) <$> Ok 2)
-- even something like this doesnt work, because we have a Ok wrapped in a Ok

-- we'd like something with a signature like
-- Ok (a -> b) -> Ok a -> Ok b
-- to apply functions in the context (the Ok) of a functor

-- we'd like to write
-- (+) <$> Just 2 <*> Just 3

-- ...AND IT WORKS! That's because
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Applicative functors
-- ...continuing with the 'container' example
-- Requires:
-- 1. a method 'pure' that takes a value, and returns a Functor containing such value.
-- 2. a method '<*>', that works like 'fmap', but
--    instead of taking a function, it receives a
--    Functor containing a function, and applies it
--    to a container of the same type
-- 3. the type constructor must be a functor, too

-- For list, <*> is concatMap, which first maps a
-- function to each list, and then concats the results.

{--
    instance Applicative [] where
    pure x = [x]
    fs <*> xs = concatMap (\f -> map f xs) fs
--}
-- example:
-- ghci> [(+1), (*2), (+10)] <*> [1,2,3]
-- [2, 3, 4, 2, 4, 6, 11, 12, 13]

-- WHY?
-- 'fmap' limits us to apply FUNCTIONS to a
-- 'container'. What if the function(s) we want to
-- apply are inside another 'container'? We can't do
-- that with functors. Hence, applicative to the rescue.
-- FUN-IN-CONT <*> VALUES-IN-CONT

-- example: pure (+) <*> [5] <*> [3]
-- Q: what does 'pure (+) <*> [5] returns?
-- A: a list of partially applied functions
-- same for [(+), (*)] <*> [5]

-- since `pure f <*> x` equals `fmap f x`
-- we can write `pure f <*> x <*> y <*> ...`
-- as `fmap f x <*> y <*> ...` or even better
-- `f <$> x <*> y <*> ...` (<$> is infix of 'fmap')