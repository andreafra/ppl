module Ex7 where

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

instance Monad Result where
  Ok x >>= f = f x
  Err >>= _ = Err

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

-- just call 'main' in ghci, it will wait for user input

-- WE are the side-effect. Main handles the side-effects (reading, writing to console) through the IO monad, which 'isolates' the impure portion of the code from the rest of the 'pure' code.