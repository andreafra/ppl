-- State Monads
-- See: https://app.eraser.io/workspace/2yRCEgLYZQLDX1DDrjQp
import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
   in pop newStack2

ex1 :: [Int]
ex1 = [1, 2, 4, 5]

{--
stackManip = do
    push 3
    a <- pop
    pop
--}

-- newtype State s a = State { runState :: s -> (a, s) }

popM :: State Stack Int
popM = state pop -- (x:xs) -> (x, xs)

pushM :: Int -> State Stack ()
pushM a = state $ \stack -> ((), a : stack)

stackManipM :: State Stack Int
stackManipM = do
  pushM 3
  a <- popM
  popM

ex2 = runState stackManipM ex1

------

data RobotState = RobotState
  { position :: (Int, Int),
    holdingObject :: Bool
  }
  deriving (Show)

moveTo :: (Int, Int) -> State RobotState ()
moveTo newPos = modify (\s -> s {position = newPos})

pickUpObject :: State RobotState ()
pickUpObject = modify (\s -> s {holdingObject = True})

-- modify f = do { x <- get ; put (f x) }

resetRobot :: State RobotState ()
resetRobot = put (RobotState (0, 0) False)

robotActions :: State RobotState ()
robotActions = do
  moveTo (3, 4)
  pickUpObject
  moveTo (1, 2)
  resetRobot

-- rs0 = RobotState (0, 0) False
rs0 = RobotState {position = (0, 0), holdingObject = False}

rs1 = runState robotActions rs0

--------------------------------

{--
Consider the binary tree data structure as seen in class.
1. Define a function btrees which takes a value x and returns an infinite list of binary trees, where:
  1. all the leaves contain x,
  2. each tree is complete,
  3. the first tree is a single leaf, and each tree has one level more than its previous one in the list.
2. Define an infinite list of binary trees, which is like the previous one, but the first leaf contains the integer 1, and each subsequent tree contains leaves that have the value of the previous one incremented by one.
E.g. [Leaf 1, (Branch (Leaf 2)(Leaf 2), ...]
3. Define an infinite list containing the count of nodes of the trees in the infinite list of the previous point. E.g. [1, 3, ...]
Write the signatures of all the functions you define.
--}

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

-- newLevel t = Branch t t

btrees :: a -> [Tree a]
btrees x = Leaf x : [Branch x x | x <- btrees x]

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

btrees' :: [Tree Int]
btrees' = Leaf 1 : [Branch ((+ 1) <$> x) ((+ 1) <$> x) | x <- btrees']

-- btrees' = Leaf 1 : [Branch ((+ 1) <$> x) ((+ 1) <$> y) | (x, y) <- zip btrees' [1 ..]]
-- TODO: How to get leaves that have incremental x?

count :: [Int]
count = [2 ^ x - 1 | x <- [1 ..]]

----------------------------

-- 1. Define a data structure, called D2L, to store lists of possibly depth two, e.g. like [1,2,[3,4],5,[6]].
-- 2. Implement a flatten function which takes a D2L and returns a flat list containing all the stored values in it in the same order.
-- 3. Make D2L an instance of Functor, Foldable, Applicative.

data D2L a = D2Nil | D2L1 a (D2L a) | D2L2 [a] (D2L a) deriving ()

d2l = D2L1 1 $ D2L1 2 $ D2L2 [3, 4] $ D2L1 5 $ D2L2 [6] D2Nil

instance (Show a) => Show (D2L a) where
  show D2Nil = ""
  show (D2L2 xs rest) = show xs ++ "," ++ show rest
  show (D2L1 x rest) = show x ++ "," ++ show rest

flatten :: D2L a -> [a]
flatten D2Nil = []
flatten (D2L1 x rest) = x : flatten rest
flatten (D2L2 xs rest) = xs ++ flatten rest

instance Functor D2L where
  fmap f D2Nil = D2Nil
  fmap f (D2L1 x rest) = D2L1 (f x) $ fmap f rest
  fmap f (D2L2 xs rest) = D2L2 (fmap f xs) (fmap f rest)

instance Foldable D2L where
  foldr f i D2Nil = i
  foldr f i (D2L1 x rest) = f x (foldr f i rest)
  foldr f i (D2L2 xs rest) = foldr f (foldr f i rest) xs

instance Applicative D2L where
  pure x = D2L1 x D2Nil

  fs <*> xs = foldr (+++) D2Nil $ fmap (\f -> fmap f xs) fs

(+++) :: D2L a -> D2L a -> D2L a
D2Nil +++ t = t
t +++ D2Nil = t
(D2L1 x rest) +++ t = D2L1 x (rest +++ t)
(D2L2 xs rest) +++ t = D2L2 xs (rest +++ t)

d2lf = D2L1 (1 +) $ D2L2 [(* 2), (^ 2)] D2Nil