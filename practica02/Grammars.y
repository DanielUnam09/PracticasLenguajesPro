{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

-- RETO 2:

    -- n-arios
    | '(' "and" ASA ASA ArgList ')'   { And ($3:$4:$5) }
    | '(' "or"  ASA ASA ArgList ')'   { Or  ($3:$4:$5) }
    | '(' '+'   ASA ASA ArgList ')'   { Add ($3:$4:$5) }
    | '(' '-'   ASA ASA ArgList ')'   { Sub ($3:$4:$5) }
    | '(' '*'   ASA ASA ArgList ')'   { Mul ($3:$4:$5) }
    | '(' '/'   ASA ASA ArgList ')'   { Div ($3:$4:$5) }
    | '(' '<'   ASA ASA ArgList ')'   { Lt  ($3:$4:$5) }
    | '(' '>'   ASA ASA ArgList ')'   { Gt  ($3:$4:$5) }
    | '(' "<="  ASA ASA ArgList ')'   { Le  ($3:$4:$5) }
    | '(' ">="  ASA ASA ArgList ')'   { Ge  ($3:$4:$5) }

    -- estrictamente binarios
    | '(' "expt" ASA ASA ')'          { Expt $3 $4 }
    | '(' "eq"   ASA ASA ')'          { EqP  $3 $4 }

    -- unarios
    | '(' "not"   ASA ')'             { Not   $3 }
    | '(' "add1"  ASA ')'             { Add1  $3 }
    | '(' "sub1"  ASA ')'             { Sub1  $3 }
    | '(' "zero?" ASA ')'             { ZeroP $3 }

-- RETO 3:
   ArgList : {- vacío -}      { [] }
           | ASA ArgList      { $1 : $2 }

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
