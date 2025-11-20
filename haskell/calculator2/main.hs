module Main where

import Calculator (loop)

main :: IO ()
main = do
  putStrLn "Welcome to the Calculator v2!"
  Calculator.loop []
