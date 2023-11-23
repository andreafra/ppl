import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Text.Read (readMaybe)

data Result a = Ok a | Err deriving (Eq, Show)

data Expr = Val Int | Div Expr Expr deriving (Eq, Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

ex1 = (Div (Val 4) (Val 2))

ex2 = Div (Div (Val 4) (Val 0)) (Val 2)

safediv :: Int -> Int -> Result Int
safediv n m =
  if m == 0
    then Err
    else Ok (n `div` m)

eval' :: Expr -> Result Int
eval' (Val n) = Ok n
eval' (Div x y) =
  case eval' x of
    Err -> Err
    Ok n -> case eval' y of
      Err -> Err
      Ok m -> safediv n m

bind :: Result Int -> (Int -> Result Int) -> Result Int
m `bind` f = case m of
  Err -> Err
  Ok x -> f x

-- Ok x `bind` f = f x
-- Err `bind` _ = Err

eval'' :: Expr -> Result Int
eval'' (Val n) = Ok n
eval'' (Div x y) =
  eval'' x `bind` (\n -> eval'' y `bind` \m -> safediv n m)

mEval :: Expr -> Result Int
mEval (Val n) = Ok n
mEval (Div x y) = do
  n <- mEval x
  m <- mEval y
  safediv n m

instance Functor Result where
  fmap f (Ok x) = Ok $ f x
  fmap _ Err = Err

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok x = f <$> Ok x
  _ <*> Err = Err
  Err <*> _ = Err

instance Monad Result where
  Ok x >>= f = f x
  Err >>= _ = Err

{--
1. LEFT IDENTITY: `return x >>= f` == `f x`
2. RIGHT IDENTITY: `m >>= return` == `m`
3. ASSOCIATIVITY: `(m >>= f) >>= g` ==
                  `m >>= (\x -> f x >>= g)
--}

greet name = "Hello " ++ name ++ "!"

main :: IO ()
-- main =
--   putStrLn "Hi..." >> (getLine >>= (\name -> putStrLn $ greet name))

main = do
  putStrLn "Hi, what's your name?"
  name <- getLine
  putStrLn $ greet name
  putStrLn "How old are you?"
  msg <- getLine
  let age = readMaybe msg :: Maybe Int
  currentTime <- getCurrentTime
  let (year, _, _) = toGregorian . utctDay $ currentTime
  case age of
    Just a -> do
      let birthYear = fromInteger year - a
      putStrLn $ "You were born in " ++ show birthYear
    _ -> putStrLn "Something went wrong!"

--------
square x = x ^ 2

addOne x = x + 1

x = addOne $ square 2

data NumberWithLogs = NumberWithLogs
  { number :: Int,
    logs :: [String]
  }
  deriving (Show)

square1 :: Int -> NumberWithLogs
square1 x = NumberWithLogs (x ^ 2) ["Squared " ++ show x ++ " to get " ++ show (x ^ 2)]

addOne1 :: NumberWithLogs -> NumberWithLogs
addOne1 x = NumberWithLogs (number x + 1) $ logs x ++ ["Added 1 to " ++ show (number x) ++ " to get " ++ show (number x + 1)]

wrapWithLogs :: Int -> NumberWithLogs
wrapWithLogs x = NumberWithLogs x []

square2 :: NumberWithLogs -> NumberWithLogs
square2 x = NumberWithLogs (number x ^ 2) $ logs x ++ ["Squared " ++ show (number x) ++ " to get " ++ show (number x ^ 2)]

runWithLogs :: NumberWithLogs -> (Int -> NumberWithLogs) -> NumberWithLogs
runWithLogs input transform =
  let newNumberWithLogs = transform (number input)
   in NumberWithLogs
        (number newNumberWithLogs)
        (logs input ++ logs newNumberWithLogs)

addOne2 :: Int -> NumberWithLogs
addOne2 x = NumberWithLogs (x + 1) ["Added 1 to " ++ show x ++ " to get " ++ show (x + 1)]

a = wrapWithLogs 2

b = runWithLogs a square1

c = b `runWithLogs` addOne2