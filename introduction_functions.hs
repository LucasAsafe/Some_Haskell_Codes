intTrinta :: Int
intTrinta = 30

boolTrue :: Bool
boolTrue = True

isGreater :: Int -> Int -> Bool
isGreater a b = a > b

getSuccessor :: Int -> Int
getSuccessor a = a + 1

isEqual :: Int -> Int -> Bool
isEqual a b = a == b

greaterThan30 :: Int -> Bool
greaterThan30 a = a > intTrinta

getMax :: Int -> Int -> Int
getMax a b = if a > b then a else b

equalValues :: Int -> Int -> Int -> Bool
equalValues a b c = (a == b) && (b == c)

returnFatorial :: Integer -> Integer
returnFatorial a = product [1..a]

productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs
