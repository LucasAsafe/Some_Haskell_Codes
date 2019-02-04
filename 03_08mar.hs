
offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

maiuscula :: Char -> Char
maiuscula ch = toEnum (fromEnum ch + offset) 

ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')

ehMinuscula :: Char -> Bool
ehMinuscula ch = ('a' <= ch) && (ch <= 'z')

-- Tuplas

primeiro :: (Int, Int) -> Int
--primeiro (x,y) = x
primeiro (x,_) = x


segundo :: (Int, Int) -> Int
segundo (x,y) = y

primeiroTripla :: (Int, Int, Int) -> Int
primeiroTripla (x, y, z) = x

shift :: ((Int,Int), Int) -> (Int,(Int,Int))
shift ((x, y), z) = (x, (y, z))

extraiValor :: (Bool,(Int,Bool),Char) -> Char
--extraiValor (b1 , (i, b2) , c) = c
extraiValor (_ , _ , c) = c

-- Sinonimos de tipos

--type Identificador = tipo

type ParInteiro = (Int,Int)

primeiroParInteiro :: ParInteiro -> Int
primeiroParInteiro (x,y) = x

type Nome = String
type Idade = Int
type Telefone = Int
type Pessoa = (Nome, Idade, Telefone)

nome :: Pessoa -> Nome
nome (n, idd, tlf) = n

-- Equacao do segundo grau

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2 * a)

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (d - e, d + e)
  where
    d = -b / (2*a)
    e = sqrt( b^2 - 4.0 * a * c) / (2.0 * a)

raizes :: Float -> Float -> Float -> String
raizes a b c
 | b ^ 2 == 4.0 * a * c = show (umaRaiz a b c)
 | b ^ 2 > 4.0 * a * c = show f ++ " " ++ show s
 | otherwise = "nao hah raizes"
  where
     (f, s)  = (duasRaizes a b c)
    -- f = fst (duasRaizes a b c) 
    -- s =  snd (duasRaizes a b c)
    