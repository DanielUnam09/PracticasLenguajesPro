module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura

-- | Variables libres de un ASA (Definicion 4 de las notas)
freeVars :: ASA -> [String]
freeVars (Id x)            = [x]
freeVars (Num _)           = []
freeVars (Boolean _)       = []
freeVars (And es)          = foldr union [] (map freeVars es)
freeVars (Or es)           = foldr union [] (map freeVars es)
freeVars (Add es)          = foldr union [] (map freeVars es)
freeVars (Sub es)          = foldr union [] (map freeVars es)
freeVars (Mul es)          = foldr union [] (map freeVars es)
freeVars (Div es)          = foldr union [] (map freeVars es)
freeVars (Lt es)           = foldr union [] (map freeVars es)
freeVars (Gt es)           = foldr union [] (map freeVars es)
freeVars (Le es)           = foldr union [] (map freeVars es)
freeVars (Ge es)           = foldr union [] (map freeVars es)
freeVars (Expt e1 e2)      = freeVars e1 `union` freeVars e2
freeVars (EqP e1 e2)       = freeVars e1 `union` freeVars e2
freeVars (Not e)           = freeVars e
freeVars (Add1 e)          = freeVars e
freeVars (Sub1 e)          = freeVars e
freeVars (ZeroP e)         = freeVars e
freeVars (Let bs body)     = foldr union [] (map (freeVars . snd) bs) 
                             `union` (freeVars body \\ map fst bs)
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((x,e):bs) body) = freeVars e `union` (freeVars (LetStar bs body) \\ [x])

-- | Todos los nombres involucrados en un ASA
names :: ASA -> [String]
names (Id x)            = [x]
names (Num _)           = []
names (Boolean _)       = []
names (And es)          = foldr union [] (map names es)
names (Or es)           = foldr union [] (map names es)
names (Add es)          = foldr union [] (map names es)
names (Sub es)          = foldr union [] (map names es)
names (Mul es)          = foldr union [] (map names es)
names (Div es)          = foldr union [] (map names es)
names (Lt es)           = foldr union [] (map names es)
names (Gt es)           = foldr union [] (map names es)
names (Le es)           = foldr union [] (map names es)
names (Ge es)           = foldr union [] (map names es)
names (Expt e1 e2)      = names e1 `union` names e2
names (EqP e1 e2)       = names e1 `union` names e2
names (Not e)           = names e
names (Add1 e)          = names e
names (Sub1 e)          = names e
names (ZeroP e)         = names e
names (Let bs body)     = map fst bs `union` foldr union [] (map (names . snd) bs) `union` names body
names (LetStar bs body) = map fst bs `union` foldr union [] (map (names . snd) bs) `union` names body

-- | Genera un nombre fresco que no este en la lista de usados
freshName :: [String] -> String
freshName used = head [ n | i <- [(1::Int)..], let n = "z" ++ show i, n `notElem` used ]

-- | Sustitucion e[x := s] (Sustitucion nominal sin captura)
sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s
  | y == x    = s
  | otherwise = Id y
sust (Num n) _ _      = Num n
sust (Boolean b) _ _  = Boolean b
sust (And es) x s     = And (map (\e -> sust e x s) es)
sust (Or es) x s      = Or (map (\e -> sust e x s) es)
sust (Add es) x s     = Add (map (\e -> sust e x s) es)
sust (Sub es) x s     = Sub (map (\e -> sust e x s) es)
sust (Mul es) x s     = Mul (map (\e -> sust e x s) es)
sust (Div es) x s     = Div (map (\e -> sust e x s) es)
sust (Lt es) x s      = Lt (map (\e -> sust e x s) es)
sust (Gt es) x s      = Gt (map (\e -> sust e x s) es)
sust (Le es) x s      = Le (map (\e -> sust e x s) es)
sust (Ge es) x s      = Ge (map (\e -> sust e x s) es)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s  = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s      = Not (sust e x s)
sust (Add1 e) x s     = Add1 (sust e x s)
sust (Sub1 e) x s     = Sub1 (sust e x s)
sust (ZeroP e) x s    = ZeroP (sust e x s)

-- Caso Let: (let ((y1 e1) ... (yk ek)) body)[x := s]
sust (Let bs body) x s
  | x `elem` xs = Let (zip xs es') body -- x esta ligada, no sustituye en body
  | null overlap = Let (zip xs es') (sust body x s) -- Sin captura
  | otherwise = -- Renombramiento de ligadores que capturarian variables libres de s
      let renombrar (y, e) (accBs, accBody, used) =
            if y `elem` fvS
            then let z = freshName used
                     accBody' = sust accBody y (Id z)
                 in ((z, e):accBs, accBody', z:used)
            else ((y, e):accBs, accBody, used)
          initUsed = names body `union` names s `union` xs
          (bsRenamed, bodyRenamed, _) = foldr renombrar ([], body, initUsed) bs
          bsFinal = map (\(y, e) -> (y, sust e x s)) bsRenamed
      in Let bsFinal (sust bodyRenamed x s)
  where
    xs = map fst bs
    es = map snd bs
    es' = map (\e -> sust e x s) es
    fvS = freeVars s
    overlap = filter (`elem` fvS) xs

-- Caso LetStar
sust (LetStar [] body) x s = LetStar [] (sust body x s)
sust (LetStar ((y, e):bs) body) x s
  | y == x = LetStar ((y, sust e x s) : bs) body
  | y `elem` fvS =
      let used = names (LetStar bs body) `union` names s `union` [x, y]
          z = freshName used
          -- Reemplaza la variable y por z en el resto del let*
          LetStar bs' body' = sust (LetStar bs body) y (Id z)
      in LetStar ((z, sust e x s) : bs') (sust body' x s)
  | otherwise =
      let LetStar bs' body' = sust (LetStar bs body) x s
      in LetStar ((y, sust e x s) : bs') body'
  where
    fvS = freeVars s

-- | Sustitucion simultanea para Let (evalua todas las asignaciones al mismo tiempo)
sustMany :: ASA -> [Binding] -> ASA
sustMany expr [] = expr
sustMany expr bs = foldl (\acc (x, v) -> sust acc x v) expr bs

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
