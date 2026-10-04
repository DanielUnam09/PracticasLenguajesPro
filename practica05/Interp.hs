module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] cuerpo = Just (Fun x cuerpo)
curryFun (x:xs) cuerpo = do
  resto <- curryFun xs cuerpo
  return (Fun x resto)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f [a] = Just (App f a)
curryApp f (a:as) = do
  resto <- curryApp (App f a) as
  return resto

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:y:xs) =
  Just (foldl op (op x y) xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa =
  desugar alternativa

desugarCond ((condicion, expresion):resto) alternativa = do
  c <- desugar condicion
  e <- desugar expresion
  r <- desugarCond resto alternativa
  return (If c e r)

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA

desugar (IdS x) =
  Just (Id x)

desugar (NumS n) =
  Just (Num n)

desugar (BooleanS b) =
  Just (Boolean b)

desugar (AddS xs) = do
  ys <- mapM desugar xs
  binaryOp Add ys

desugar (SubS xs) = do
  ys <- mapM desugar xs
  binaryOp Sub ys

desugar (NotS e) = do
  e' <- desugar e
  return (Not e')

desugar (LetS x e cuerpo) = do
  e' <- desugar e
  cuerpo' <- desugar cuerpo
  return (App (Fun x cuerpo') e')

desugar (LetStarS [] cuerpo) =
  desugar cuerpo

desugar (LetStarS ((x, e):resto) cuerpo) =
  desugar
    (LetS
      x
      e
      (LetStarS resto cuerpo))

desugar (FunS params cuerpo) = do
  cuerpo' <- desugar cuerpo
  curryFun params cuerpo'

desugar (AppS f args) = do
  f' <- desugar f
  args' <- mapM desugar args
  curryApp f' args'

desugar (IfS condicion entonces sino) = do
  condicion' <- desugar condicion
  entonces' <- desugar entonces
  sino' <- desugar sino
  return (If condicion' entonces' sino')

desugar (CondS clausulas alternativa) =
  desugarCond clausulas alternativa

desugar (LetRecS f definicion cuerpo) =
  desugar
    (LetS
      f
      (AppS
        (IdS "Y")
        [FunS [f] definicion])
      cuerpo)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] =
  Nothing

lookupEnv x ((y, valor):resto)
  | x == y    = Just valor
  | otherwise = lookupEnv x resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV expresion ambiente) = do
  valor <- bigStep ambiente expresion
  strict valor

strict valor =
  Just valor

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value

bigStep _ (Num n) =
  Just (NumV n)

bigStep _ (Boolean b) =
  Just (BooleanV b)

bigStep ambiente (Id x) =
  lookupEnv x ambiente

bigStep ambiente (Fun parametro cuerpo) =
  Just (ClosureV parametro cuerpo ambiente)

bigStep ambiente (Add izquierda derecha) = do
  vi <- bigStep ambiente izquierda >>= strict
  vd <- bigStep ambiente derecha >>= strict

  case (vi, vd) of
    (NumV n, NumV m) ->
      Just (NumV (n + m))
    _ ->
      Nothing

bigStep ambiente (Sub izquierda derecha) = do
  vi <- bigStep ambiente izquierda >>= strict
  vd <- bigStep ambiente derecha >>= strict

  case (vi, vd) of
    (NumV n, NumV m) ->
      Just (NumV (max 0 (n - m)))
    _ ->
      Nothing

bigStep ambiente (Not expresion) = do
  valor <- bigStep ambiente expresion >>= strict

  case valor of
    BooleanV b ->
      Just (BooleanV (not b))
    _ ->
      Nothing

bigStep ambiente (If condicion entonces sino) = do
  valor <- bigStep ambiente condicion >>= strict

  case valor of
    BooleanV True ->
      bigStep ambiente entonces
    BooleanV False ->
      bigStep ambiente sino
    _ ->
      Nothing

bigStep ambiente (App funcion argumento) = do
  valorFuncion <- bigStep ambiente funcion >>= strict

  case valorFuncion of
    ClosureV parametro cuerpo ambienteDefinicion ->
      bigStep
        ((parametro, ExprV argumento ambiente) : ambienteDefinicion)
        cuerpo
    _ ->
      Nothing
