
-- Ejercicio 1 Distancia Euclidiana al origen
distanciaOrigen :: Double -> Double -> Double 
distanciaOrigen x y = sqrt((x)^2 + (y)^2)

-- Ejercicio 2 Cuadrados elementos pares de una lista

sumaCuadradoPares :: [Int] -> Int
sumaCuadradoPares xs =  sum (map ( ^2) (filter even xs))

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
                       | (temperatura <= 0) = "frio extremo"
                       | (temperatura > 0 && temperatura <= 18) = "frio"
                       | (temperatura > 18 && temperatura <= 23) = "templado"
                       | (temperatura > 23 && temperatura <= 32) = "calido" 
                       | otherwise = "calor extremo"


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

-- Creación de el evaluador algebraico para Expr
evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma expr1 expr2) = evalua expr1 + evalua expr2
evalua (Producto expr1 expr2) = evalua expr1 * evalua expr2

 

