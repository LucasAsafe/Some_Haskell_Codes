
answer :: Int
answer = 30

compara30 :: Int -> Bool
compara30 x = x > answer

comparaDoisArgs x y = x > y

quadrado :: Int -> Int
quadrado x = x * x 

aplicaF :: (Int -> Int) -> Int -> Int
aplicaF f x = f x 

todosIguais :: Int -> Int -> Int -> Bool
todosIguais  a b c = (a == b) && (b == c)

maxi :: Int -> Int -> Int
maxi x y
  | x >= y = x
  | otherwise = y

