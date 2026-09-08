module Laboratorio01 where

-- Ejercicio 1 Distancia Euclidiana al origen
distanciaOrigen :: Double -> Double -> Double 
distanciaOrigen x y = sqrt((x)^2 + (y)^2)

-- Ejercicio 2 Cuadrados elementos pares de una lista

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs =  sum (map ( ^2) (filter even xs))

-- Ejercicio 3 Aplica una función 3 veces al mismo valor. 

aplicaTresVeces :: (a -> a) -> a -> a 
aplicaTresVeces f x = (f (f (f x)))

-- Ejercicio 4 Saca el primedio de los parametros de entrada y los usa para sacar la varianza 
varianza2 :: Double -> Double -> Double 
varianza2 datoUno datoDos = 
          let mediaLocal  =  (datoUno + datoDos) / 2
          in ((datoUno - mediaLocal)^2 + (datoDos - mediaLocal)^2) / 2

-- Ejercicio 5 Usa guards para selecionar la temperatura correcta.
clasificaTemperatura :: Int -> String
clasificaTemperatura temperatura 
             | temperatura <= 0  = "frio extremo"
             | temperatura <= 15 = "frio"
             | temperatura <= 25 = "templado"
             | temperatura <= 35 = "calido" 
             | otherwise    = "calor extremo"


-- Ejercicio 6 Separador de elementos usando recuersión 
intercala :: a -> [a] -> [a]
intercala nuevoVal []  = []
intercala nuevoVal [x] = [x]
intercala nuevoVal (y:ys) =  y :  nuevoVal : intercala  nuevoVal ys

-- Ejercicio 7 Creación de data y evaluación algebraica. 

-- Creación de data Expr 
data Expr = Lit Int
          | Suma Expr Expr 
          | Producto Expr Expr
          deriving (Eq, Show)

-- Creación de el evaluador algebraico para Expr
evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma expr1 expr2) = evalua expr1 + evalua expr2
evalua (Producto expr1 expr2) = evalua expr1 * evalua expr2


recorrer :: [a] -> [a]
recorrer [] = []
recorrer (x : xs) filtra x [a] : recorrer xs

filtra :: a -> [a] 
filtra [] = a : [a]
filtra (x : xs) = 
    if x == a
    then ()
    else filtra xs

 

