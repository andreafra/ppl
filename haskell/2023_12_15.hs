{- 2023-01-25
We want to define a data structure for the tape of a Turing machine: Tape is a parametric data structure with respect to the tape content, and must be made of three components:
1. the portion of the tape that is on the left of the head;
2. the symbol on which the head is positioned;
3. the portion of the tape that is on the right of the head.
Also, consider that the machine has a concept of "blank" symbols, so you need to add another component in the data definition to store the symbol used to represent the blank in the parameter type.
1. Define Tape.
2. Make Tape an instance of Show and Eq, considering that two tapes contain the same values if their stored values are the same and in the same order, regardless of the position of their heads.
3. Define the two functions left and right, to move the position of the head on the left and on the right.
4. Make Tape an instance of Functor and Applicative. -}

data Tape a = Tape [a] a [a] a

t1 = Tape [1, 0] 2 [3, 4] 0

t2 = Tape [2, 1, 0] 3 [4] 0

instance (Show a) => Show (Tape a) where
  show (Tape l h r b) = show (reverse l) ++ show h ++ show r

tapelist :: Tape a -> [a]
tapelist (Tape l g r _) = reverse l ++ [g] ++ r

instance (Eq a) => Eq (Tape a) where
  a == b = tapelist a == tapelist b

left :: Tape a -> Tape a
left (Tape [] h r b) = Tape [] b (h : r) b
left (Tape (x : xs) h r b) = Tape xs x (h : r) b

right :: Tape a -> Tape a
right (Tape l h [] b) = Tape (h : l) b [] b
right (Tape l h (x : xs) b) = Tape (h : l) x xs b

-- if it was not reversed:
-- right (Tape l h (x : xs) b) = Tape (l ++ [h]) x xs b

instance Functor Tape where
  fmap f (Tape l h r b) = Tape (fmap f l) (f h) (fmap f r) (f b)

instance Applicative Tape where
  pure a = Tape [] a [] a
  (Tape lf hf rf rb) <*> (Tape xl xh xr xb) = Tape (lf <*> xl) (hf xh) (rf <*> xr) (rb xb)

tf = Tape [(+ 1), (* 2)] (+ 2) [(+ 1)] id