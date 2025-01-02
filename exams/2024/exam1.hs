-- 2023-07-03
{-- 
1. Define a data structure, called D2L, to store lists of possibly depth two, e.g. like [1,2,[3,4],5,[6]].
2. Implement a flatten function which takes a D2L and returns a flat list containing all the stored values in it in the same order.
3. Make D2L an instance of Functor, Foldable, Applicative. --}

-- 1
data D2L a = D2LNil | D2L1 a (D2L a) | D2L2 [a] (D2L a) deriving (Show, Eq)

--        D2L                 List []
-- 2 [1,2,[3,4],5,[6]] -> [1,2,3,4,5,6]
flatten D2LNil = []
flatten (D2L1  x xs) = x : flatten xs
flatten (D2L2 xs ys) = xs ++ flatten xs

-- 3
instance Functor D2L where
    fmap f D2LNil = D2LNil
    fmap f (D2L1 x xs) = D2L1 (f x) (fmap f xs)
    fmap f (D2L2 xs ys) = D2L2 (fmap f xs) (fmap f ys)

instance Foldable D2L where
    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f z D2LNil = z
    foldr f z (D2L1 x xs) = f x (foldr f z xs)
    foldr f z (D2L2 xs ys) = (foldr f (foldr f z ys) xs)

-- custom concatenate function
D2LNil +++ t = t
t +++ D2LNil = t
(D2L1 x xs) +++ t = D2L1 x (xs +++ t)
(D2L2 xs ys) +++ t = D2L2 xs (ys +++ t)

instance Applicative D2L where
    pure x = D2L1 x D2LNil

    fs <*> xs = foldr (+++) D2LNil (fmap (\f -> fmap f xs) fs)

