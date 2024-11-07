module Example1 where

import Data.Foldable (foldl')

hello = "Ciao!"

y = x + 1 -- => 4

x = 3

greet name = "Hello " ++ name

listOfNumbers = [1 .. 5]

data Food = Fruit | Dairy | Fish | Meat

dish = Fruit

-- LHS: TYPE CONSTRUCTOR
-- RHS: DATA CONSTRUCTOR
data Point2d a = P2D a a

p1 = P2D 0 0

p2 = P2D 4 5

manhattanDist :: Num a => Point2d a -> Point2d a -> a
manhattanDist
  (P2D x0 y0)
  (P2D x1 y1) = (x1 - x0) + (y1 - y0)

data Point3D a = Point3D
  { pointX :: a,
    pointY :: a,
    pointZ :: a
  }

getX Point3D {pointX = x} = x

--- TREES

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)

mytree = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))

---         X     Y     X+Y
-- sum :: Int -> Int -> Int

--          Y     Y+1
-- add1 :: Int -> Int
add1 = (1 +)

--   = \x -> 1 + x

cylinderArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

cylinderArea' r h = sideArea + 2 * topArea
  where
    sideArea = 2 * pi * r * h
    topArea = pi * r ^ 2

myInfiniteListOfEvenNums =
  [x * 2 | x <- [0, 1 ..]]

---

foldlNotLazyRes =
  foldlNotLazy (\x y -> 1) 0 [2, 3]

foldlNotLazy f z [] = z
foldlNotLazy f z (x : xs) =
  z `seq` foldlNotLazy f (f z x) xs
