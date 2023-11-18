-- import Ex4Lib (sumThree)

-- myFunc :: (Num a) => a -> a -> a

-- class Eq a where
--   (==) :: a -> a -> Bool
--   x /= y = not (x == y)

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

-- instance (Eq a) => Eq (Tree a) where
--   Leaf a == Leaf b = a == b
--   (Branch l1 r1) == (Branch l2 r2) = l1 == l2 && r1 == r2
--   _ == _ = False

-- instance (Show a) => Show (Tree a) where
--   show (Leaf a) = show a
--   show (Branch l r) = "<" ++ show l ++ " | " ++ show r ++ ">"

treeFoldr f z Empty = z
treeFoldr f z (Leaf x) = f x z
treeFoldr f z (Branch l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = treeFoldr

foldl f a bs = foldr (\b g x -> g (f x b)) id bs a

data Result a = Err | Ok a deriving (Eq, Ord, Show)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok $ n `div` m

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Ok x) = Ok $ f x
  fmap _ Err = Err

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

instance Applicative Result where
  (<*>) :: Result (a -> b) -> Result a -> Result b
  (Ok f) <*> x = f <$> x
  _ <*> Err = Err
  Err <*> _ = Err

  pure = Ok

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = concatMap (\f -> map f xs) fs

-- if you declare a tree with this syntax (compared to the other tree we've defined) you'll get a different `show` result! Isn't that handy?
data MyTree a
  = MyLeaf a
  | MyNode
      { left :: MyTree a,
        value :: a,
        right :: MyTree a
      }
  deriving (Show)

-- t = MyNode (MyLeaf 1) 2 (MyNode (MyLeaf 3) 4 (MyLeaf 5)))
-- MyNode {left = MyLeaf 1, value = 2, right = MyNode {left = MyLeaf 3, value = 4, right = MyLeaf 5}}