x = 5

greet :: String -> String
greet name = "Hello " ++ name

howLong :: [a] -> Integer
howLong [] = 0
howLong (x : xs) = 1 + howLong xs

-- SUM/UNION
data Food = Fruit | Dairy | Fish | Meat

-- PRODUCT TYPES
data Point2d a = Point2d a a

manhattanDistance
  (Point2d x0 y0)
  (Point2d x1 y1) = (x1 - x0) + (y1 - y0)

p1 = Point2d 0 0

p2 = Point2d 1 1

data Point2d' a = Point2d' {pointX, pointY :: a}

p3 = Point2d' 1 2

getJustX Point2d' {pointX = x, pointY = y} = y

manhattanDistance'
  a
  b = (pointX a - pointX b) + (pointY a - pointY b)

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

myTree = Branch (Leaf 'a') 'c' (Branch (Leaf 'b') 'c' (Leaf 'd'))

lst = (1 : (2 : (4 : [])))

add1 = (1 +)

double = (2 *)

myList = [1, 2, 3, 4, 5]

cylinderArea :: Float -> Float -> Float
cylinderArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

s = sum (map (\x -> x * 2) myList)

s' = sum $ map (* 2) myList

showMyList lst@(x : xs) =
  "Head: "
    ++ show x
    ++ " Rest: "
    ++ show xs
    ++ " List: "
    ++ show lst

infList = [1, 3 ..]

finiteList = [1 .. 10]

evenNums = [x * 2 | x <- [0, 1 ..]]

-- a ^ 2 + b ^ 2 = c ^ 2
pytagoreanTriples =
  [ (a, b, c)
    | c <- [1 ..],
      b <- [1 .. c],
      a <- [1 .. b],
      a ^ 2 + b ^ 2 == c ^ 2
  ]

-- BMI = weight / height ^ 2
calcBMI weight height
  | weight / height ^ 2 <= 18.5 = "Underweight"
  | weight / height ^ 2 < 25 = "Normal"
  | weight / height ^ 2 < 30 = "Overweight"
  | otherwise = "Obese"

calcBMI2 weight height
  | bmi <= underw = "Underweight"
  | bmi < normal = "Normal"
  | bmi < overw = "Overweight"
  | otherwise = "Obese"
  where
    bmi = weight / height ^ 2
    (underw, normal, overw) = (18.5, 25, 30)

initials firstName lastName = [f] ++ " " ++ [l]
  where
    (f : _) = firstName
    (l : _) = lastName

ifExample x = [1, if x > 2 then 999 else 0]

head' :: [a] -> a
head' [] = error "Cant get head"
head' (x : _) = x

head'' xs = case xs of
  [] -> error "no head"
  (x : _) -> x
