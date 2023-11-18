module Ex8 where

import Control.Monad.State

-- Define a type for snacks and their prices
data Snack = Snack
  { snackName :: String,
    snackPrice :: Int,
    snackQuantity :: Int
  }
  deriving (Show)

-- Define the vending machine state
data VendingMachineState = VendingMachineState
  { inventory :: [Snack], -- List of snacks and their quantities
    depositedMoney :: Int -- Amount of money deposited by the user
  }
  deriving (Show)

-- Define a type synonym for the State monad with VendingMachineState
type VendingMachine a = State VendingMachineState a

-- Function to add a snack to the vending machine inventory
addSnack :: Snack -> VendingMachine ()
addSnack snack = do
  state <- get
  -- get current inventory, append it to new snack
  let updatedInventory = snack : inventory state
  put $ state {inventory = updatedInventory}

-- Function to deposit money into the vending machine
depositMoney :: Int -> VendingMachine ()
depositMoney amount = do
  state <- get
  let updatedMoney = depositedMoney state + amount
  put $ state {depositedMoney = updatedMoney}

-- Function to purchase a snack
purchaseSnack :: String -> VendingMachine (Maybe Snack)
purchaseSnack snackId = do
  state <- get
  let snacks = [(snackName snack, snack) | snack <- inventory state]
  case lookup snackId snacks of
    Just
      Snack
        { snackName = name,
          snackPrice = price,
          snackQuantity = quantity
        }
        | quantity > 0,
          depositedMoney state > price -> do
            -- Update state: decrement quantity and deduct money
            let updatedInventory = Snack name price (quantity - 1) : filter (\s -> snackId /= snackName s) (inventory state)
                updatedMoney = depositedMoney state - price
            put $
              state
                { inventory = updatedInventory,
                  depositedMoney = updatedMoney
                }
            return $ Just $ Snack name price 1
    _ -> return Nothing

-- Example vending machine operations
exampleVendingMachine :: VendingMachine (Maybe Snack)
exampleVendingMachine = do
  addSnack (Snack "Chips" 50 5)
  addSnack (Snack "Soda" 75 10)
  depositMoney 100
  purchaseSnack "Chips"

main :: IO ()
main = do
  putStrLn "Initial State:"
  print initialState
  let (result, finalState) = runState exampleVendingMachine initialState
  putStrLn "Final State:"
  print finalState
  putStrLn "Result of Purchase:"
  print result

-- Initial vending machine state with no inventory and no deposited money
initialState :: VendingMachineState
initialState = VendingMachineState {inventory = [], depositedMoney = 0}
