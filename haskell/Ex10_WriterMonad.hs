import Control.Monad.Trans.Writer

-- taking the example about the NumberWithLogs
-- that was a Writer Monad

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number " ++ show x])

multWithLogs = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Multiply " ++ show a ++ " and " ++ show b]
  return (a * b)

-- we use `runWriter` to get a tuple out of the monad
numAndLogs = runWriter $ do
  a <- multWithLogs
  tell ["Just a random log"] -- try to swap with with return
  return a

-- Careful with Writer's Log type, because it's a monoid
-- implementing `mappend` which is basically the operation
-- of the monoid. In case of [] it's ++, which is slow if
-- we always append at the END of a Linked List, but fast
-- if we append at the BEGINNING.

-- SMALL NOTE ABOUT FUNCTIONS AND MONADS
-- functions are monads too...

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

-- addStuff 3
-- WHAT'S THE RESULT? (19) NOT (16)
-- WHY?

-- instance Monad ((->) r) where
--     return x = \_ -> x
--     h >>= f = \w -> f (h w) w

-- It's like
addStuff' :: Int -> Int
addStuff' x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b