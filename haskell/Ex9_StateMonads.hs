import Control.Monad.State

-- By default, in haskell, it's not very easy to
-- change a (global) state of your application.
-- In other programming languages, you simply
-- change some value x = 5; x = 6; x = 0...

-- The state monad helps us to do that!

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

-- it works... but we need to keep track of the stack all
-- the time. That's kinda boring. Wouldn't you prefer:
-- stackManip = do
--     push 3
--     a <- pop
--     pop

-- STATE = STATEFUL COMPUTATION

-- STATE IS DEFINED AS:
-- newtype State s a = State {runState :: s -> (a, s)}
-- manipulates s
-- returns a

-- THE STATE MONAD IS IMPLEMENTED AS:
-- instance Monad (State s) where
--     return x = State $ \s -> (x,s)
--     (State h) >>= f = State $ \s ->
--         let (a, newState) = h s
--             (State g) = f a
--         in  g newState
--
-- The result of feeding a (stateful computation) to a
-- function must be a function (again)

popM :: State Stack Int
popM = state $ \(x : xs) -> (x, xs)

pushM :: Int -> State Stack ()
pushM a = state $ \xs -> ((), a : xs)

stackManipM :: State Stack Int
stackManipM = do
  pushM 3
  a <- popM
  popM

-- Another Example

data RobotState = RobotState
  { position :: (Int, Int),
    holdingObject :: Bool
  }
  deriving (Show)

moveTo :: (Int, Int) -> State RobotState ()
moveTo newPos = modify (\s -> s {position = newPos})

pickUpObject :: State RobotState ()
pickUpObject = modify (\s -> s {holdingObject = True})

robotActions :: State RobotState ()
robotActions = do
  moveTo (3, 4)
  pickUpObject
  moveTo (1, 2)

state0 =
  RobotState
    { position = (0, 0),
      holdingObject = False
    }

state1 = runState robotActions state0

-- We also have `get` and `put`
-- get = state $ \s -> (s,s) :: State s s
-- put newState = state $ \s -> ((), newState) :: s -> State s ()
-- modify f = do { x <- get ; put (f x) } :: (s -> s) -> State s ()