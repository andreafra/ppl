module Ex2024_11_15 where

-- class Eq a where
--     (==) :: a -> a -> Bool
--     x /= y = not (x == y)

data Tree a
  = Empty
  | Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Show)

instance (Eq a) => Eq (Tree a) where
  Empty == Empty = True
  (Leaf a) == (Leaf b) = a == b
  (Branch l1 r1) == (Branch l2 r2) =
    (l1 == l2) && (r1 == r2)
  _ == _ = False

-- class (Eq a) => Ord a where
--     (<), (<=), (>=), (>) :: a -> a -> Bool
--     max, min :: a -> a -> a

-- instance (Show a) => Show (Tree a) where
--   show Empty = "()"
--   show (Leaf a) = show a
--   show (Branch l r) =
--     "<" ++ show l ++ "|" ++ show r ++ ">"

myTree =
  Branch
    ( Branch
        (Leaf 1)
        Empty
    )
    ( Branch
        Empty
        (Leaf 2)
    )

--
data Result a = Err | Ok a
  deriving (Eq, Ord, Show)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok $ n `div` m

instance Functor Result where
  fmap f (Ok a) = Ok $ f a
  fmap _ Err = Err

-- 1. fmap id = id
-- 2. fmap ( f . g ) = fmap f . fmap g

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch l r) =
    -- Branch (f <$> l) (f <$> r)
    Branch (fmap f l) (fmap f r)

-- Result (Int -> Int) -> Result Int -> Result Int
-- f (a -> b) -> f a -> f b

instance Applicative Result where
  -- pure puts a value inside Result
  pure x = Ok x -- why is pure Err => Err

  -- apply
  Ok f <*> Ok x = Ok $ f x
  Err <*> _ = Err
  _ <*> Err = Err

-- 1. pure id <*> v = v
-- 2. pure f <*> pure x = pure (f x)
-- 3. u <*> pure y = pure ($ y) <*> u
-- 4. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

treeFoldr f z Empty = z
treeFoldr f z (Leaf x) = f x z
treeFoldr f z (Branch l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = treeFoldr
