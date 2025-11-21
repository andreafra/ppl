{-- 2025.02.06
Make a Circular List (the last node points to the first node).
Add a sentinel node (a node that does not contain any data), used
to check if we have travered the whole list.
An empty list contains only the sentinel node, pointing to itself.
--}

data Clist a
  = Node a (Clist a) -- data node
  | End (Clist a) -- sentinel node

instance (Show a) => Show (Clist a) where
  show (End _) = "..."
  show (Node v next) = show v ++ ", " ++ show next

instance (Eq a) => Eq (Clist a) where
  End _ == End _ = True
  (Node x next) == (Node x' next') = (x == x') && (next == next')
  _ == _ = False

-- Can we use 'deriving' to implement automatically Show and Eq?

instance Functor Clist where
  fmap f (Node x next) =
    let first = Node (f x) $ fmap' f next first
     in first
    where
      fmap' f (End x) first = End first
      fmap' f (Node x next) first = Node (f x) $ fmap' f next first

instance Foldable Clist where
  foldr f z clist = foldr' clist
    where
      foldr' (Node x next) = f x (foldr' next)
      foldr' (End _) = z

instance Applicative Clist where
  pure x =
    let first = Node x $ End first
     in first
  (Node f fs) <*> (Node x xs) =
    let first = Node (f x) $ apply' fs xs first
     in first
    where
      apply' (End t) _ first = End first
      apply' _ (End t) first = End first
      apply' (Node f fs) (Node x xs) first = Node (f x) $ apply' fs xs first

cconcat :: Clist (Clist a) -> Clist a
cconcat (End _) = let e = End e in e
cconcat clists =
  let empty = let e = End e in e
      append xs ys = foldr Node ys xs
   in foldr append empty clists

instance Monad Clist where
  clist >>= f = cconcat (fmap f clist)

exampleClist :: Clist Int
exampleClist = Node 1 $ Node 2 $ Node 3 $ End exampleClist

exampleClist2 :: Clist Int
exampleClist2 = Node 4 $ Node 5 $ Node 6 $ End exampleClist2

exampleMonadicListComprehension = do
  x <- exampleClist
  y <- exampleClist2
  return (x + y)
