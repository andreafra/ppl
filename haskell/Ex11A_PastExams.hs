module Ex11A_PastExams where

{-- 2025.06.16 --
Consider the following datatype definition.
data PTL a = P a a | T a a a | L [PTL a] deriving (Show, Eq)
Describe its possible usage, then make PTL an instance of Functor, Foldable, and Applicative.
--}

data PTL a = P a a | T a a a | L [PTL a] deriving (Show, Eq)

instance Functor PTL where
  fmap f (P x y) = P (f x) (f y)
  fmap f (T x y z) = T (f x) (f y) (f z)
  fmap f (L xs) = L (map (fmap f) xs)

-- Why not... fmap f (L xs) = L (fmap f xs)
-- fmap f (L xs) :: (a -> b) -> f a -> f b
-- fmap f (L xs) :: (PTL a -> PTL b) -> L [PTL a] -> L [PTL b]
-- The issue is that L contains a list of PTL a, not a list of a.
-- So, if we just did: L (fmap f xs), we'd be calling the functor of a generic list,
-- when we actually need to get a list of PTL b, hence, we apply `fmap f`
-- to each element of the list to get a PTL b out of every PTL a.

instance Foldable PTL where
  foldr f z (P x1 x2) = f x1 (f x2 z)
  foldr f z (T x1 x2 x3) = f x1 (f x2 (f x3 z))
  foldr f z (L xs) = foldr (\ptl acc -> foldr f acc ptl) z xs

instance Applicative PTL where
  pure x = P x x
  P f g <*> P x y = P (f x) (g y)
  T f g h <*> T x y z = T (f x) (g y) (h z)
  L fs <*> L xs = L [f <*> x | f <- fs, x <- xs]

-- instead of the 'zipped' version:
-- L fs <*> L xs = L (appZip fs xs)
--   where
--     appZip [] _ = []
--     appZip _ [] = []
--     appZip (f : fs) (x : xs) = (f <*> x) : appZip fs xs
