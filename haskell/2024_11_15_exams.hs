{--
Consider the binary tree data structure as seen in class.
1. Define a function btrees which takes a value x and
returns an infinite list of binary trees, where:
1. all the leaves contain x,
2. each tree is complete,
3. the first tree is a single leaf, and each tree has one level more than its previous one in the list.

2. Define an infinite list of binary trees, which is like the
previous one, but the first leaf contains the integer 1, and each
subsequent tree contains leaves that have the value of the previous
one incremented by one.
E.g. [Leaf 1, (Branch (Leaf 2)(Leaf 2), ...]

3. Define an infinite list containing the count of nodes
of the trees in the infinite list of the previous point.
E.g. [1, 3, ...]

Write the signatures of all the functions you define.
--}

data BTree a = Leaf a | Branch (BTree a) (BTree a) deriving (Show)

addLevel subtree = Branch subtree subtree

btrees x = (Leaf x) : [addLevel t | t <- btrees x]

-- 2
instance Functor BTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

btrees2 = (Leaf 1) : [(+ 1) <$> Branch t t | t <- btrees2]

-- 3
countTrees = 1 : [x * 2 + 1 | x <- countTrees]
