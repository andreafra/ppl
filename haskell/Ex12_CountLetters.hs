module Ex12_CountLetters where

import Data.Array (Array, array, (!), (//))
import Data.Char
import Data.Map (empty, findWithDefault, insert)

w1 = "banana"

w2 = "abcdefg"

w3 = "i love haskell"

{--
Define a function that returns the occurrences
of each letter for the given word.
Skip spaces. Case insensitive.
--}

countWithDict w =
  countH w Data.Map.empty
  where
    countH [] m = m
    countH (' ' : xs) m = countH xs m -- skip spaces
    countH (x : xs) m =
      let k = Data.Char.toLower x
          currValue = Data.Map.findWithDefault 0 k m
          newValue = currValue + 1
          newMap = Data.Map.insert k newValue m
       in countH xs newMap

countWithArray :: String -> Array Int Int
countWithArray w = countH w emptyArray
  where
    aInt = ord 'a'
    emptyArray = array (0, 25) [(i, 0) | i <- [0 .. 25]]

    countH [] arr = arr
    countH (x : xs) arr
      | not (isAlpha x) = countH xs arr
      | otherwise =
          let k = ord (toLower x) - aInt
              currValue = arr ! k
              newArr = arr // [(k, currValue + 1)]
           in countH xs newArr
