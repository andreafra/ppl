-- In Haskell, we write our code in modules
-- The entry point of a program is the 'Main'
-- module, with a function called 'main'
module Ex1 where

-- this is a variable assignment
hello = "Hello!"
-- ATTENTION: variables in Haskell are immutable.
{-- example: this code works. It also doesn't matter the order which variables are defined in.
y = x + 1
x = 2
--}

-- optionally, we can declare the type of a function
greet :: String -> String
greet name = "Hello " ++ name ++ "!"

-- we can have 'TYPE variables', aka placeholders
-- for any type in our function: '[a]' can be a list of any type
-- Also, functions are defined through PATTERN MATCHING
howLong :: [a] -> Integer
howLong [] = 0 -- match base case: empty list => 0
howLong (x : xs) = 1 + howLong xs -- match iterative case
-- here, 'x' is the 'car', and 'xs' is the 'cdr'
-- this is VERY handy!
