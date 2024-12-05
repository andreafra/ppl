module Ex2024_11_25 where

import Text.Read (readMaybe)

data Expr
  = Val Int
  | Div Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = div (eval x) (eval y)

data Result a = Err | Ok a deriving (Eq, Show)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok $ div n m -- n/m

eval' :: Expr -> Result Int
eval' (Val n) = Ok n
eval' (Div x y) =
  case eval' x of
    Err -> Err
    Ok n -> case eval' y of
      Err -> Err
      Ok m -> safediv n m

bind :: Result a -> (a -> Result b) -> Result b
m `bind` f = case m of
  Err -> Err
  Ok x -> f x

eval'' :: Expr -> Result Int
eval'' (Val n) = Ok n
eval'' (Div x y) =
  eval'' x
    `bind` ( \n ->
               eval'' y `bind` (\m -> safediv n m)
           )

instance Functor Result where
  fmap f (Ok x) = Ok $ f x
  fmap _ Err = Err

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok x = Ok $ f x
  _ <*> Err = Err
  Err <*> _ = Err

instance Monad Result where
  -- return = pure
  -- bind: >>=
  Ok x >>= f = f x
  Err >>= _ = Err

evalM :: Expr -> Result Int
evalM (Val n) = Ok n
evalM (Div x y) = do
  n <- evalM x
  m <- evalM y
  safediv n m

testOk = evalM $ Div (Val 3) (Val 1)

testErr = evalM $ Div (Val 3) (Val 0)

-- return x >>= f equals to f x
-- right identity `m >>= return` equals `m`
-- associativity (m >>= f) >>= g
--               m >>= (\x -> f x >>= g)
--
--
-- f1 :: Int -> Int -> Int

-- f2 :: Int -> Int -> IO Int

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hi, what's your name?"
  line <- getLine
  let greeting = greet line
  putStrLn greeting
  -- Ask your age
  putStrLn "How old are you?"
  line <- getLine
  let age = readMaybe line :: Maybe Int
  case age of
    Just a -> putStrLn $ "You are " ++ show a
    Nothing -> putStrLn "It's not a number!"
