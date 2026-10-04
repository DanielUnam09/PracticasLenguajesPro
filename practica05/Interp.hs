
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
curryFun (x:xs) cuerpo =
  curryFun xs cuerpo >>= \resultado ->
  Just (Fun x resultado)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f [a] = Just (App f a)
curryApp f (a : as) =
  curryApp (App f a) as

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (a : b : xs) =
  Just (foldl op (op a b) xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa =
  desugar alternativa

desugarCond ((condicion, rama) : clausulas) alternativa =
  desugar condicion >>= \c ->
  desugar rama >>= \r ->
  desugarCond clausulas alternativa >>= \resto ->
  Just (If c r resto)

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.


desugar :: SASA -> Maybe ASA
desugar (IdS nombre) =
  Just (Id nombre)

desugar (NumS n) =
  Just (Num n)

desugar (BooleanS b) =
  Just (Boolean b)

desugar (AddS operandos) =
  mapM desugar operandos >>= binaryOp Add

desugar (SubS operandos) =
  mapM desugar operandos >>= binaryOp Sub

desugar (NotS expresion) =
  desugar expresion >>= \e ->
  Just (Not e)

desugar (LetS nombre definicion cuerpo) =
  desugar definicion >>= \d ->
  desugar cuerpo >>= \c ->
  Just (App (Fun nombre c) d)

desugar (LetStarS bindings cuerpo) =
  desugarLetStar bindings cuerpo

desugar (FunS parametros cuerpo) =
  desugar cuerpo >>= curryFun parametros

desugar (AppS funcion argumentos) =
  desugar funcion >>= \f ->
  mapM desugar argumentos >>= curryApp f

desugar (IfS condicion consecuente alternativa) =
  desugar condicion >>= \c ->
  desugar consecuente >>= \t ->
  desugar alternativa >>= \e ->
  Just (If c t e)

desugar (CondS clausulas alternativa) =
  desugarCond clausulas alternativa

desugar (LetRecS nombre definicion cuerpo) =
  desugar
    (LetS nombre
      (AppS (IdS "Y") [FunS [nombre] definicion])
      cuerpo)

desugarLetStar :: [(Nombre, SASA)] -> SASA -> Maybe ASA
desugarLetStar [] cuerpo =
  desugar cuerpo

desugarLetStar ((nombre, definicion) : bindings) cuerpo =
  desugarLetStar bindings cuerpo >>= \c ->
  desugar definicion >>= \d ->
  Just (App (Fun nombre c) d)

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
strict (ExprV expresion ambiente) =
  bigStep ambiente expresion >>= strict

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

numValue :: Value -> Maybe Int
numValue (NumV n) = Just n
numValue _        = Nothing

boolValue :: Value -> Maybe Bool
boolValue (BooleanV b) = Just b
boolValue _            = Nothing

closureParts :: Value -> Maybe (Nombre, ASA, Env)
closureParts (ClosureV p cuerpo ambienteDef) = Just (p, cuerpo, ambienteDef)
closureParts _                               = Nothing

numOperando :: Env -> ASA -> Maybe Int
numOperando ambiente e =
  bigStep ambiente e >>= strict >>= numValue

bigStep :: Env -> ASA -> Maybe Value

bigStep _ (Num n) =
  Just (NumV n)

bigStep _ (Boolean b) =
  Just (BooleanV b)

bigStep ambiente (Id x) =
  lookupEnv x ambiente

bigStep ambiente (Fun parametro cuerpo) =
  Just (ClosureV parametro cuerpo ambiente)

bigStep ambiente (Add izquierda derecha) =
  (\n m -> NumV (n + m)) <$> numOperando ambiente izquierda <*> numOperando ambiente derecha

bigStep ambiente (Sub izquierda derecha) =
  (\n m -> NumV (max 0 (n - m))) <$> numOperando ambiente izquierda <*> numOperando ambiente derecha

bigStep ambiente (Not expresion) =
  BooleanV . not <$> (bigStep ambiente expresion >>= strict >>= boolValue)


bigStep ambiente (If condicion entonces sino) =
  (bigStep ambiente condicion >>= strict >>= boolValue) >>= escogeRama
  where
    escogeRama True  = bigStep ambiente entonces
    escogeRama False = bigStep ambiente sino

bigStep ambiente (App funcion argumento) =
  (bigStep ambiente funcion >>= strict >>= closureParts) >>= aplica
  where
    aplica (parametro, cuerpo, ambienteDefinicion) =
      bigStep ((parametro, ExprV argumento ambiente) : ambienteDefinicion) cuerpo