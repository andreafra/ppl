{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
module Ex7 where

import Data.Time (UTCTime (utctDay), getCurrentTime)
import Data.Time.Calendar
import Text.Read (readMaybe)

{-- Let's test what we've learned by
solving this issue:
- we define a data type to represent numbers `Val`
- we define a data type to represent divisions `Div`
- `eval` is a function that evaluates a
  math expression defined with Val and Div
 --}

data Expr
  = Val Int
  | Div Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- What if we divide by ZERO?
-- Spoiler: we crash! => Exception: divide by zero

-- How can we make division by 0 it SAFE?
-- Let's define safediv which CAN'T fail!

-- first, let's define a simple "box" to hold
-- our value if it's ok, or an 'error' if it's
-- the result of something invalid
data Result a = Err | Ok a
  deriving (Eq, Ord, Show)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok (n `div` m)

eval' :: Expr -> Result Int
eval' (Val n) = Ok n
eval' (Div x y) = case eval' x of
  Err -> Err
  Ok n -> case eval' y of
    Err -> Err
    Ok m -> safediv n m

-- too verbose: we're repeating the same structure...
-- how can we improve it?

-- let's define a 'bind' function that:
-- - takes a Result 'm' and
-- - takes a function 'f' that take
--   the value inside the Result m
--   and returns a new Result
-- - returns the result of 'f' if the value of 'm'
--   is not Err, otherwise returns Err.
bind :: Result a -> (a -> Result b) -> Result b
m `bind` f = case m of
  Err -> Err
  Ok x -> f x

eval'' :: Expr -> Result Int
eval'' (Val n) = Ok n
-- 'return' (:: Int -> m Int) is the generic version of
-- 'Ok' (:: Int -> Result Int), which puts 'n'
-- into the 'Maybe' "box" in this case.
--
-- We can then use the 'sequence' operator >>=
eval'' (Div x y) =
  eval'' x
    `bind` ( \n ->
               eval'' y
                 `bind` ( \m ->
                            safediv n m
                        )
           )

-- we've removed the 'case' and the 'Result/Ok/Err'
-- part, but it still does not look great...
-- what can Haskell do for us?

-- The 'DO' notation (syntactic sugar)
-- [You need a Monad for this]
{--
    mEval :: Expr -> Result Int
    mEval (Val n) = Ok n
    mEval (Div x y) = do
    n <- mEval x
    m <- mEval y
    safediv n m
--}

-- Let's redefine Result as a Monad
-- It needs to be an 'applicative' instance
-- So it means it also needs to be a 'functor' instance

instance Functor Result where
  fmap f (Ok x) = Ok (f x)
  fmap _ Err = Err

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  _ <*> Err = Err -- 'whatever' applied to an Err is an Err
  Err <*> _ = Err

instance Monad Result where
  Ok x >>= f = f x
  Err >>= _ = Err

-- we don't define return because return = pure by default!

mEval :: Expr -> Result Int
mEval (Val n) = Ok n
mEval (Div x y) = do
  n <- mEval x
  m <- mEval y
  safediv n m

testOk = mEval $ Div (Val 3) (Val 1)

testErr = mEval $ Div (Val 3) (Val 0)

-- what we've just done is redefining the Maybe monad
-- Simply swap:
-- Result -> Maybe, Ok -> Just and Err -> Nothing.

-- MONADS HAVE RULES!
-- 1. LEFT IDENTITY: `return x >>= f` is the same as `f x`
-- 2. RIGHT IDENTITY: `m >>= return` is the same as `m`
-- 3. ASSOCIATIVITY: Doing `(m >>= f) >>= g` is like doing
--                   `m >>= (\x -> f x >>= g)`

-- It bridges the values from the PURE world to the
-- IMPURE world.
-- Unfortunately, you can't really write real programs
-- without side-effect. Haskell lets you relegate
-- side-effects to monads.

-- SAME IDEA, works for other EFFECTS:
-- I/O, Mutable State, Networking, Error Handling...

-- Hello World (as a monad)

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

main = do
  putStrLn "Hi, what's your name?"
  msg <- greet <$> getLine
  putStrLn msg
  putStrLn "How old are you?"
  msg <- getLine
  -- we can use let to declare local values (in the do block)
  -- NOTE that we don't use `in`
  let age = readMaybe msg :: Maybe Int
  -- Getting the Date is an IO operation.
  -- We need to handle the IO monad again!
  currentTime <- getCurrentTime
  let (year, _, _) = toGregorian . utctDay $ currentTime
  case age of
    Just a -> do
      let birthYear = fromInteger year - a
      putStrLn $ "You were born in " ++ show birthYear
    _ -> putStrLn "Something went wrong!"

-- just call 'main' in ghci, it will wait for user input

-- WE are the side-effect. Main handles the side-effects (reading, writing to console) through the IO monad, which 'isolates' the impure portion of the code from the rest of the 'pure' code.

-- Fun Fact: The IO Monad is implemented as a State Monad, where the State that get passed around is the real world!

-- ANOTHER EXAMPLE OF MONADS FROM SCRATCH:
-- (This works if you want to reimplement monads in other languages)
square x = x ^ 2

addOne x = x + 1

x = addOne $ square 2

-- we'd like the have a log of all operations performed:
data NumberWithLogs = NumberWithLogs
  { number :: Int,
    logs :: [String]
  }
  deriving (Show)

square1 :: Int -> NumberWithLogs
square1 x = NumberWithLogs (x ^ 2) ["Squared " ++ show x ++ " to get " ++ show (x ^ 2)]

addOne1 :: NumberWithLogs -> NumberWithLogs
addOne1 x = NumberWithLogs (number x + 1) $ logs x ++ ["Added 1 to " ++ show (number x) ++ " to get " ++ show (number x + 1)]

-- of course, we can't swap these two operations
-- solutions?
wrapWithLogs :: Int -> NumberWithLogs
wrapWithLogs x = NumberWithLogs x []

square2 :: NumberWithLogs -> NumberWithLogs
square2 x = NumberWithLogs (number x ^ 2) $ logs x ++ ["Squared " ++ show (number x) ++ " to get " ++ show (number x ^ 2)]

-- improvements?

runWithLogs :: NumberWithLogs -> (Int -> NumberWithLogs) -> NumberWithLogs
runWithLogs input transform =
  let newNumberWithLogs = transform (number input)
   in NumberWithLogs (number newNumberWithLogs) (logs input ++ logs newNumberWithLogs)

-- (we need to reimplement addOne :: Int -> NumberWithLogs)
addOne2 :: Int -> NumberWithLogs
addOne2 x = NumberWithLogs (x + 1) ["Added 1 to " ++ show x ++ " to get " ++ show (x + 1)]

a = wrapWithLogs 2

b = runWithLogs a square1

c = b `runWithLogs` addOne2

-- What is `wrapWithLogs` in a monad context? (return)
-- What is `runWithLogs` in a monad context? (bind)
-- Are Monad Laws valid? (yes)