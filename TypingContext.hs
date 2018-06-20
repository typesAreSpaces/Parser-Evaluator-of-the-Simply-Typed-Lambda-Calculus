module TypingContext where

import Syntax

type TypingContext = [(String, Binding)]

-- Both getValue and getName will return 
-- the last element added
getValue :: TypingContext -> String -> Integer
getValue xs s = f 0 xs s where
  f xs []          s = -1 -- This -1 will be useful for the pickfreshname function
  f n  ((y, _):ys) s = if (y == s) then n else f (n + 1) ys s

getName :: TypingContext -> Integer -> String
getName xs n = g 0 xs n where
  g _ []          _ = error "no name defined"
  g i ((y, _):ys) n = if (i == n) then y else g (i + 1) ys n

getType :: TypingContext -> Integer -> Type
getType xs n = g 0 xs n where
  g _ []                   _ = error "no type defined"
  g i ((_, VarBind ty):ys) n = if (i == n) then ty else g (i + 1) ys n
  g i ((_, NameBind):ys)   n = g (i + 1) ys n

update :: TypingContext -> String -> Type -> TypingContext
update gamma s ty = (s, VarBind ty) : gamma

update2 :: TypingContext -> String -> TypingContext
update2 gamma s = (s, NameBind) : gamma

pickfreshname :: TypingContext -> String -> String
pickfreshname gamma fresh = if (getValue gamma fresh == -1) 
  then fresh
  else pickfreshname gamma (fresh++"'")

getContext :: Term -> TypingContext
getContext (VarTerm x)               = [(x, NameBind)]
getContext (AbsTerm x ty t1)         = (x, VarBind ty) : getContext t1
getContext (AppTerm t1 t2)           = getContext t1 ++ getContext t2
getContext TrueTerm                  = []
getContext FalseTerm                 = []
getContext (IfThenElseTerm t1 t2 t3) = getContext t1 ++ getContext t2 ++ getContext t3
getContext ZeroTerm                  = []
getContext (SuccTerm t1)             = getContext t1
getContext (PredTerm t1)             = getContext t1
getContext (IsZeroTerm t1)           = getContext t1
