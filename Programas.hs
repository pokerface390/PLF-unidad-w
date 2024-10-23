-------------------------Funciones Básicas------------------------
promedio_c :: Fractional a => a -> a -> a -> a
promedio_c x y z = (x + y + z) / 3

sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20

volumenEsfera :: Floating a => a -> a
volumenEsfera r = (4/3) * pi * r^3

areaDeCoronaCircular :: Floating a => a -> a -> a
areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)

ultimaCifra :: Integral a => a -> a
ultimaCifra x = x `rem` 10

maxTres :: Ord a => a -> a -> a -> a
maxTres x y z = max x (max y z)

rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

rango :: Ord a => [a] -> [a]
rango xs = [minimum xs, maximum xs]

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

interior :: [a] -> [a]
interior xs = tail (init xs)

segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = take (n - m + 1) (drop m xs)

extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs

mediano :: (Ord a, Num a) => a -> a -> a -> a
mediano x y z = x + y + z - minimum [x, y, z] - maximum [x, y, z]

tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales x y z = x == y && y == z

tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = x /= y && y /= z && x /= z

cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = tresIguales x y z && z == u

------------------------- Guardas y Patrones ------------------------
divisionSegura :: Double -> Double -> Double
divisionSegura _ 0 = 9999
divisionSegura x y = x / y

xor1 :: Bool -> Bool -> Bool
xor1 True False = True
xor1 False True = True
xor1 _ _ = False

mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo r1@(b1, h1) r2@(b2, h2)
  | b1 * h1 >= b2 * h2 = r1
  | otherwise = r2

intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

ciclo :: [a] -> [a]
ciclo [] = []
ciclo [x] = [x]
ciclo xs = last xs : init xs

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = max (10 * x + y) (10 * y + x)

numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
  | discriminante > 0 = 2
  | discriminante == 0 = 1
  | otherwise = 0
  where discriminante = b^2 - 4 * a * c

raices :: Double -> Double -> Double -> [Double]
raices a b c
  | discriminante < 0 = []
  | discriminante == 0 = [-b / (2 * a)]
  | otherwise = [(-b + sqrt discriminante) / (2 * a), (-b - sqrt discriminante) / (2 * a)]
  where discriminante = b^2 - 4 * a * c

area :: Double -> Double -> Double -> Double
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a, b] [x, y]
  | b < x || a > y = []
  | otherwise = [max a x, min b y]

linea :: Integer -> [Integer]
linea n = [inicio..fin]
  where inicio = (n * (n - 1)) `div` 2 + 1
        fin = inicio + n - 1

---------------------------Recursividad -----------------------

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = (x == y) || pertenece x ys

tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 _ = []
tomar n (x:xs) = x : tomar (n - 1) xs

digitosC :: Integer -> [Integer]
digitosC 0 = [] 
digitosC n = digitosC (n `div` 10) ++ [n `mod` 10] 


sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida [y | y <- xs, y <= x] 
                    ++ [x] 
                    ++ ordenaRapida [y | y <- xs, y > x]

------------------------------Nuevos tipos de datos------------------------

data Estudiante = Estudiante String String Int Int deriving (Show, Eq, Ord)

getEdad :: Estudiante -> Int
getEdad (Estudiante _ _ edad _) = edad

quicksort :: [Estudiante] -> [Estudiante]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort izq ++ [x] ++ quicksort der
  where
    izq = [y | y <- xs, getEdad y <= getEdad x]
    der = [y | y <- xs, getEdad y > getEdad x]

estudianteMenor :: [Estudiante] -> Estudiante
estudianteMenor [] = error "No hay estudiantes"
estudianteMenor estudiantes = head (quicksort estudiantes)

estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor [] = error "No hay estudiantes"
estudianteMayor estudiantes = last (quicksort estudiantes)

promedioEdades :: [Estudiante] -> Float
promedioEdades estudiantes = fromIntegral (sum (map getEdad estudiantes)) / fromIntegral (length estudiantes)

------------------------------Árboles------------------------------------

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

generaNodo :: a -> Arbol a
generaNodo x = Nodo x Hoja Hoja

insertar :: (Ord a) => a -> Arbol a -> Arbol a
insertar x Hoja = generaNodo x
insertar x (Nodo a izq der)
    | x < a     = Nodo a (insertar x izq) der
    | x > a     = Nodo a izq (insertar x der)
    | otherwise = Nodo a izq der

insertarDesdeArreglo :: (Ord a) => [a] -> Arbol a -> Arbol a
insertarDesdeArreglo [] arbol = arbol
insertarDesdeArreglo (x:xs) arbol = insertarDesdeArreglo xs (insertar x arbol)

preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo x izq der) = [x] ++ preorden izq ++ preorden der

enorden :: Arbol a -> [a]
enorden Hoja = []
enorden (Nodo x izq der) = enorden izq ++ [x] ++ enorden der

postorden :: Arbol a -> [a]
postorden Hoja = []
postorden (Nodo x izq der) = postorden izq ++ postorden der ++ [x]

