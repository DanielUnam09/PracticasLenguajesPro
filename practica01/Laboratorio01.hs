
-- Ejercicio 1 Distancia Euclidiana al origen
distanciaOrigen :: Double -> Double -> Double 
distanciaOrigen x y = sqrt((x)^2 + (y)^2)

-- Ejercicio 2 Cuadrados elementos pares de una lista

sumaCuadradoPares :: [Int] -> Int
sumaCuadradoPares xs =  sum (map ( ^2) (filter even xs))

-- Ejercicio 3 