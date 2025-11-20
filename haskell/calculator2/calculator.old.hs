module Calculator (loop) where

import Text.Read (readMaybe)

data Input
  = Num (Maybe Double)
  | Op (Double -> Double -> Double)
  | Compute
  | Exit

loop :: [Input] -> IO ()
loop ops = do
  input <- getLine
  putStrLn $ "You entered: " ++ input
  let x = parse input
  -- it's the same: x <- return $ parse input

  case x of
    Compute -> putStrLn $ "The result is: " ++ show (compute ops)
    Exit -> putStrLn "Exiting calculator. Bye!"
    _ -> loop $ ops ++ [x]

-- Parse a string into a command for the calculator.
parse :: String -> Input
parse "+" = Op (+)
parse "-" = Op (-)
parse "*" = Op (*)
parse "/" = Op (/)
parse "=" = Compute
parse "exit" = Exit
parse x = Num (readMaybe x :: Maybe Double)

compute :: [Input] -> Double
compute [] = 0
compute [Num (Just x)] = x
compute ((Num (Just x)) : (Op f) : (Num (Just y)) : xs) =
  compute $ Num (Just $ f x y) : xs
compute _ = error "invalid input"
