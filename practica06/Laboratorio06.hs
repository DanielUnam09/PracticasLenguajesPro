module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaAux base exponente 1

potenciaAux :: Int -> Int -> Int -> Int
potenciaAux _ 0 acc = acc
potenciaAux base exponente acc =
  potenciaAux base (exponente - 1) (acc * base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k =
  potenciaCPS base (exponente - 1) (\r -> k (base * r))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudAux xs 0

longitudAux :: [a] -> Int -> Int
longitudAux [] acc = acc
longitudAux (_ : xs) acc = longitudAux xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_ : xs) k = longitudCPS xs (\n -> k (1 + n))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

-- Recursion de cola
reversaCola :: [a] -> [a]
reversaCola xs = reversaAux xs []
  where
    reversaAux [] acc = acc
    reversaAux (x : xs) acc =
      reversaAux xs (x : acc)

-- Continuaciones (CPS)
reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k =
  reversaCPS xs (\res -> k (res ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

-- Recursion de cola
concatenaCola :: [[a]] -> [a]
concatenaCola xss = concatenaAux xss []
  where
    concatenaAux [] acc = reversaCola acc
    concatenaAux (xs : xss) acc =
      concatenaAux xss (agregaReversa xs acc)

    agregaReversa [] acc = acc
    agregaReversa (x : xs) acc =
      agregaReversa xs (x : acc)

-- Continuaciones (CPS)
concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k =
  concatenaCPS xss (\res -> k (xs ++ res))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

-- La recursion con Cola
intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = aux xs ys []
  where
    -- El acumulador guarda los elementos al reves
    aux [] ysdos acc = vaciarAcumulador acc ysdos
    aux xsdos [] acc = vaciarAcumulador acc xsdos
    aux (x:xsdos) (y:ysdos) acc =
      aux xsdos ysdos (y : x : acc)

    -- Invertimos el acumulador frente al resto
    vaciarAcumulador [] resto = resto
    vaciarAcumulador (z:zs) resto =
      vaciarAcumulador zs (z : resto)

-- El paso con Continuaciones (CPS)
intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k =
  intercalaCPS xs ys (\res -> k (x : y : res))
