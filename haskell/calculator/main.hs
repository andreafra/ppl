module Main where

import Calculator

main :: IO ()
main = do
  putStrLn "Welcome to the Calculator!"
  Calculator.loop []
