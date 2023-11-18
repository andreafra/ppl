module Ex2 where

{-- USER-DEFINED TYPES

Let's make a new type 'Food'
It can have different values, which will be specific foods:
Fruit, Dairy, Fish and Meat.
We call:
'Food' => Type Constructor
'Fruit', 'Dairy', ... => Data Constructors
--}

-- sum/union type
data Food = Fruit | Dairy | Fish | Meat

-- PRODUCT TYPES - like a struct
-- data Point2D a = Point a a
-- NOTE: Type Constr. and Data Constr. reside in separate namespaces. It means that we can call them both the same:
data Point2d a = Point2d a a

-- access is positional
-- let's calc the manhattan distance for example:
manhattanDistance
  (Point2d x0 y0)
  (Point2d x1 y1) = (x1 - x0) + (y1 - y0)

-- An alternative way to access data fields is to use
-- the following syntax:
data Point2dAlt a = Point2dAlt {pointX, pointY :: a}

-- which gives us two 'selector functions':
-- - pointX p -> x
-- - pointY p -> y

-- pattern match against specific values of a record
getJustX Point2dAlt {pointX = x} = x

-- RECURSIVE-TYPES
-- we can also have recursive types - 🌲
data Tree a = Leaf a | Branch (Tree a) (Tree a)

myTree = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))

--   /\
--  a /\
--   b  c

-- TYPE SYNONYMS - aliases for types to improve legibility
type MyString = [Char] -- a string is just a list of chars