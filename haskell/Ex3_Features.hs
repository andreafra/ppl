module Ex3 where

-- CURRYING
-- function have only 1 parameter
-- we're talking about partial evaluation:

-- some partially applied functions
add1 = (1 +) -- it's like: add1 x = 1 + x

double = (2 *)

myList = [1, 2, 3, 4, 5]

-- map f lst allows us to apply f to each elem in lst
-- map add1 myList ; => [2, 3, 4, 5, 6]

-- we can COMPOSE FUNCTIONS with '.'
-- map (add1 . double) myList
-- functions are applied RIGHT to LEFT:
-- map (double . add1) myList returns a different result!

-- we can reduce the number of parenthesis needed with the

-- LET - is equal to (letrec* ...) of Racket
-- it lets (haha) us define variables in a LOCAL scope.
cylinderArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- $ symbol:
-- it's defined as:
-- ($) :: (a -> b) [f] -> a [x] -> b [result]
-- f $ x = f x
-- EXAMPLE: sum $ map (\x -> x * 2) [1, 2, 3, 4, 5] => 30
-- VS       sum (map (\x -> x * 2) [1, 2, 3, 4, 5])

-- $ basically says: "do first what's after me"
-- btw, (\x -> x * 2) is a lambda expression

-- another symbol you could see sometimes it's the '@'
-- it's for DESTRUCTURING in pattern matching
-- it lets us keep the reference to the whole list,
-- the head, and the rest. You can also use it with 'structs'
-- aka the product datatypes we've seen before.
showMyList lst@(x : xs) =
  "Head: "
    ++ show x
    ++ " Rest: "
    ++ show xs
    ++ " List: "
    ++ show lst

-- (INFINITE) LISTS 🤯
take5 = take 5 [1 ..] -- => [1, 2, 3 ...]
-- of course, [1..5] generates [1,2,3,4,5]

-- List comprehension:
-- ever used them in Python?
evenNumbers = [x * 2 | x <- [0, 1 ..]]

-- We can use List Comprehensions to generate multiple
-- values at once in a very elegant way:
pytagoreanTriples =
  [ (a, b, c)
    | c <- [1 ..],
      b <- [1 .. c],
      a <- [1 .. b],
      a ^ 2 + b ^ 2 == c ^ 2
  ]

-- ZIP : take in parallel one element from each of two lists
-- and makes a pair out of them
-- zip [1, 2, 3, 4, 5] ["hello"]

-- BOOLEAN GUARDS
{-- let's make a function that calculates the Body Mass Index
BMI = weight (kg) / (height (m) * height (m))
BMI Categories:
    Underweight = <18.5
    Normal weight = 18.5–24.9
    Overweight = 25–29.9
    Obesity = BMI of 30 or greater
--}
calcBMI weight height
  | weight / height ^ 2 <= 18.5 = "Underweight"
  | weight / height ^ 2 < 25.0 = "Normal"
  | weight / height ^ 2 < 30 = "Overweight"
  | otherwise = "Obese"

-- works, but does it looks great? If only there was a way
-- to group weight / height...

calcBMI2 weight height
  | bmi <= underweight = "Underweight"
  | bmi < normal = "Normal"
  | bmi < overweight = "Overweight"
  | otherwise = "Obese"
  where
    bmi = weight / height ^ 2
    -- using pattern matching for assignment
    (underweight, normal, overweight) = (18.5, 25.0, 30.0)

-- another small example of pattern matching
-- careful: we're returning a string, a list of chars, and
-- '++' concatenates strings! Wrap chars in []
initials firstName lastName = [f] ++ " " ++ [l]
  where
    (f : _) = firstName
    (l : _) = lastName

-- WHERE vs LET:
-- 'where' is just syntactic construct,
-- 'let' are expressions themselves
-- so you can write:
-- let square x = x * x in (square 5, square 3, square 2)]

-- IFs - not much to say
-- they are expression, too
ifExample x = [1, if x > 2 then 999 else 0]

-- CASE
-- pattern matching on parameters in function definitions
-- is just syntactic sugar for pattern matching.
-- again, they are expressions so we can combine them with
-- other stuff.

head' :: [a] -> a
head' [] = error "Cannot get head for Empty List!"
head' (x : _) = x

-- is equal to
head'' xs = case xs of
  [] -> error "No Head!"
  (x : xs) -> x

-- Higher-Order Functions
-- FOLDL, FOLDR, MAP, FILTER
-- they work just like in scheme
{-- Strict Foldl: Seq computes the Accumulator at each step
foldl' f z (x:xs) =
  let z' = f z x in z' `seq` foldl' f z' xs

  TRY THIS:
  foldl (\x y -> 1) undefined [2] -- => 1
  vs
  foldl' (\x y -> 1) undefined [2] -- => error
--}

{-- COUPLE OF WORDS OF ADVICE from "Real World Haskell"
Without some direction, there is an element of mystery to using seq effectively. Here are some useful rules for using it well.
To have any effect, a seq expression must be the first thing evaluated in an expression.

-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
hiddenInside x y = someFunc (x ‘seq‘ y)

-- incorrect: a variation of the above mistake
hiddenByLet x y z = let a = x ‘seq‘ someFunc y
                    in anotherFunc a z
-- correct: seq will be evaluated first, forcing evaluation of x
onTheOutside x y = x ‘seq‘ someFunc y
To strictly evaluate several values, chain applications of seq together.
--}