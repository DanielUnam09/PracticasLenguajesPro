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
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise = do
      rest <- curryFun xs e
      Just (Fun x rest)

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f (a:as) = Just (foldl App (App f a) as)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []  = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:y:rest) = Just (foldl op (op x y) rest)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS n)      = Just (Id n)
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)

desugar (AddS ops) = do
  ops' <- traverse desugar ops
  binaryOp Add ops'

desugar (SubS ops) = do
  ops' <- traverse desugar ops
  binaryOp Sub ops'

desugar (NotS e) = do
  e' <- desugar e
  Just (Not e')

desugar (LetS x e1 e2) = do
  e1' <- desugar e1
  e2' <- desugar e2
  Just (App (Fun x e2') e1')

desugar (LetStarS bindings body) = desugar (anidLets bindings body)
  where
    anidLets [] b            = b
    anidLets [(x, e)] b      = LetS x e b
    anidLets ((x, e):bs) b   = LetS x e (anidLets bs b)

desugar (FunS params e) = do
  e' <- desugar e
  curryFun params e'

desugar (AppS f args) = do
  f'    <- desugar f
  args' <- traverse desugar args
  curryApp f' args'

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv = lookup

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n)     = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Id x)    = lookupEnv x env

bigStep env (Add a b) = do
  NumV x <- bigStep env a
  NumV y <- bigStep env b
  Just (NumV (x + y))

bigStep env (Sub a b) = do
  NumV x <- bigStep env a
  NumV y <- bigStep env b
  Just (NumV (max 0 (x - y)))

bigStep env (Not e) = do
  v <- bigStep env e
  case v of
    BooleanV b -> Just (BooleanV (not b))
    NumV _     -> Just (BooleanV False)
    _          -> Nothing

bigStep env (Fun x e) = Just (ClosureV x e env)

bigStep env (App f a) = do
  ClosureV p body defEnv <- bigStep env f
  v <- bigStep env a
  bigStep ((p, v) : defEnv) body
