module Ex4 where

import Ex4Lib (sumThree)

-- Importing other modules:
x = sumThree 3

-- ADT - Abstract Data Type
-- operations on ADT are done at an abstract level
-- representation-independent

-- Type Classes (Ad-hoc Polymorphism = Overloading)
-- es.: '5' might be a Float, an Integer, etc.

{-- value equality is defined as:
class Eq a where
    (==) :: a -> a -> Bool
a TYPE can be an INSTANCE of a CLASS meaning it IMPLEMENTS
its OPERATIONS with METHODS.
Remember: Classes are usually abstract concepts, but we can define default implementation too:
    x /= y = not (x == y)
-- 'not equal' is simply the negation of equality, by definition. We just have to implement '==' to get '/=' for free.
--}
data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where -- 'Tree a' is an 'instance' of 'Eq a', meaning it implements '=='
  Leaf a == Leaf b = a == b -- this is a 'method'
  -- we can also write: (prefix notation - works ONLY for SYMBOLS, no [a-zA-Z0-9])
  -- (==) (Leaf a) (Leaf b) = a == b
  (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2) -- this is the same 'method' again, we're doing pattern matching
  _ == _ = False -- ditto

{-- Subclasses can be defined as:
class (Eq a) => Ord a where
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
--}

-- Implementing the 'Show a' class
-- it's basically the toString() of Java
-- try printing Tree without implementing Show, it will return an error
instance (Show a) => Show (Tree a) where
  show (Leaf a) = show a
  show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"

-- DERIVING implementation from classes
data Tree' a = Leaf' a | Branch' (Tree' a) (Tree' a)
  deriving (Show, Eq)

-- Example of custom class?
