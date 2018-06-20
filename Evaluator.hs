module Evaluator where

import Syntax
import DeBruijnIndex
import TypingContext

isNumericValue :: DeBruijnExpr -> Bool
isNumericValue (ZeroTerm')   = True
isNumericValue (SuccTerm' x) = isNumericValue x
isNumericValue _             = False

isValue :: DeBruijnExpr -> Bool
isValue (TrueTerm')  = True
isValue (FalseTerm') = True
isValue (AbsTerm' _) = True
isValue x            = isNumericValue x

eval1 :: DeBruijnExpr -> Maybe DeBruijnExpr -- one-step reduction, return Nothing if not reducible
eval1 (AppTerm' t1 t2) 
  | isValue t1 = case (isValue t2) of
                  True  ->  case t1 of 
                    AbsTerm' t12 -> Just (shift (-1) (substitution (VarTerm' 0) (shift 1 t2) t12))              -- E-AppAbs
                    _ -> Nothing
                  False -> eval1 t2 >>= (\t2' -> Just (AppTerm' t1 t2'))                                        -- E-App2
  | otherwise = eval1 t1 >>= (\t1' -> Just (AppTerm' t1' t2))                                                   -- E-App1
eval1 (IfThenElseTerm' TrueTerm' t2 t3)              = Just t2                                                  -- E-IfTrue
eval1 (IfThenElseTerm' FalseTerm' t2 t3)             = Just t3                                                  -- E-IfFalse
eval1 (IfThenElseTerm' t1 t2 t3)                     = eval1 t1 >>= (\t1' -> Just (IfThenElseTerm' t1' t2 t3))  -- E-If
eval1 (SuccTerm' t1)                                 = eval1 t1 >>= (\t1' -> Just (SuccTerm' t1'))              -- E-Succ
eval1 (PredTerm' ZeroTerm')                          = Just ZeroTerm'                                           -- E-PredZero
eval1 (PredTerm' (SuccTerm' x))   | isNumericValue x = Just x                                                   -- E-PredSucc
eval1 (PredTerm' t1)                                 = eval1 t1 >>= (\t1' -> Just (PredTerm' t1'))              -- E-Pred
eval1 (IsZeroTerm' ZeroTerm')                        = Just TrueTerm'                                           -- E-IszeroZero
eval1 (IsZeroTerm' (SuccTerm' x)) | isNumericValue x = Just FalseTerm'                                          -- E-IszeroSucc
eval1 (IsZeroTerm' t1)                               = eval1 t1 >>= (\t1' -> Just (IsZeroTerm' t1'))            -- E-Iszero
eval1 _                                              = Nothing                                                  -- When no rules are applicable

eval :: DeBruijnExpr -> DeBruijnExpr  -- multi-step reduction (iterates eval1 as many times as possible)
eval t = case (eval1 t) of
          Nothing -> t
          Just t' -> eval t'

evalTerm :: Term -> Term
evalTerm t = restorenames context $ eval $ removenames context t where
  context = getContext t
