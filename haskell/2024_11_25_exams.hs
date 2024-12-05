module Exams2024_11_25 where

-- EXAMS
-- 2024-01-11
{--
Consider the following type of expressions, containing variables
of some type a, values that are integers, and some kind of binary operator
called Op.

data Expr a = Var a | Const Int | Op (Expr a) (Expr a)

1) Make it an instance of Functor, Applicative, and Monad.
2) Using an example, show what the >>= operator does in your implementation.
--}

data Expr a = Var a | Const Int | Op (Expr a) (Expr a)

instance Functor Expr where
  fmap _ (Const x) = Const x
  fmap g (Var x) = Var $ g x
  fmap g (Op a b) = Op (fmap g a) (fmap g b)

instance Applicative Expr where
  pure = Var :: a -> Expr a
  Const x <*> _ = Const x
  Var f <*> Var x = Var $ f x
  Var f <*> Op x y = Op (fmap f x) (fmap f y)
  Op f g <*> x = Op (f <*> x) (g <*> x)

instance Monad Expr where
  Const x >>= _ = Const x
  Var x >>= f = f x
  Op g h >>= f = Op (g >>= f) (h >>= f)

-- Var 3 >>= \x -> Var (x * 2) => Var 6
-- Const 1 >>= \x -> ... => Const 1
-- Op (Var 1) (Const 1) >>= \x -> Var (x + 1) => Op (Var 2) (Const 1)
