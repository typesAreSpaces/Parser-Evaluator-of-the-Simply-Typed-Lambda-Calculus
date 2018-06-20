module DeBruijnIndex where

import Syntax
import TypingContext

-- Nameless representation of Lambda Terms
data DeBruijnExpr = VarTerm' Integer
    | AbsTerm' DeBruijnExpr
    | AppTerm' DeBruijnExpr DeBruijnExpr
    | TrueTerm' 
    | FalseTerm'
    | IfThenElseTerm' DeBruijnExpr DeBruijnExpr DeBruijnExpr
    | ZeroTerm'
    | SuccTerm' DeBruijnExpr
    | PredTerm' DeBruijnExpr
    | IsZeroTerm' DeBruijnExpr deriving (Eq, Show)

removenames :: TypingContext -> Term -> DeBruijnExpr
removenames gamma (VarTerm x)               = VarTerm' $ getValue gamma x
removenames gamma (AbsTerm x ty e)          = AbsTerm' (removenames (update gamma x ty) e)
removenames gamma (AppTerm e1 e2)           = AppTerm' (removenames gamma e1) (removenames gamma e2)
removenames gamma TrueTerm                  = TrueTerm'
removenames gamma FalseTerm                 = FalseTerm'
removenames gamma (IfThenElseTerm e1 e2 e3) = IfThenElseTerm' (removenames gamma e1) (removenames gamma e2) (removenames gamma e3)
removenames gamma ZeroTerm                  = ZeroTerm'
removenames gamma (SuccTerm e1)             = SuccTerm' (removenames gamma e1)
removenames gamma (PredTerm e1)             = PredTerm' (removenames gamma e1)
removenames gamma (IsZeroTerm e1)           = IsZeroTerm' (removenames gamma e1)

restorenames :: TypingContext -> DeBruijnExpr -> Term
restorenames gamma (VarTerm' n)               = VarTerm $ getName gamma n
restorenames gamma (AbsTerm' e)               = AbsTerm omega BoolType (restorenames (update2 gamma omega) e)
  where omega = pickfreshname gamma "x"
restorenames gamma (AppTerm' e1 e2)           = AppTerm (restorenames gamma e1) (restorenames gamma e2)
restorenames gamma TrueTerm'                  = TrueTerm
restorenames gamma FalseTerm'                 = FalseTerm
restorenames gamma (IfThenElseTerm' e1 e2 e3) = IfThenElseTerm (restorenames gamma e1) (restorenames gamma e2) (restorenames gamma e3)
restorenames gamma ZeroTerm'                  = ZeroTerm
restorenames gamma (SuccTerm' e1)             = SuccTerm (restorenames gamma e1)
restorenames gamma (PredTerm' e1)             = PredTerm (restorenames gamma e1)
restorenames gamma (IsZeroTerm' e1)           = IsZeroTerm (restorenames gamma e1)

shift :: Integer -> DeBruijnExpr -> DeBruijnExpr
shift = shiftC 0
  where
    shiftC c d (VarTerm' k)               = if (k < c) then VarTerm' k else VarTerm' (k + d)
    shiftC c d (AbsTerm' t1)              = AbsTerm' (shiftC (c + 1) d t1)
    shiftC c d (AppTerm' t1 t2)           = AppTerm' (shiftC c d t1) (shiftC c d t2)
    shiftC _ _ TrueTerm'                  = TrueTerm'
    shiftC _ _ FalseTerm'                 = FalseTerm'
    shiftC c d (IfThenElseTerm' e1 e2 e3) = IfThenElseTerm' (shiftC c d e1) (shiftC c d e2) (shiftC c d e3)
    shiftC _ _ ZeroTerm'                  = ZeroTerm'
    shiftC c d (SuccTerm' e1)             = SuccTerm' (shiftC c d e1)
    shiftC c d (PredTerm' e1)             = PredTerm' (shiftC c d e1)
    shiftC c d (IsZeroTerm' e1)           = IsZeroTerm' (shiftC c d e1)

substitution :: DeBruijnExpr -> DeBruijnExpr -> DeBruijnExpr -> DeBruijnExpr
substitution (VarTerm' j) s (VarTerm' k)               = if (k == j) then s else VarTerm' k
substitution (VarTerm' j) s (AbsTerm' t1)              = AbsTerm' (substitution (VarTerm' (j + 1)) (shift 1 s) t1)
substitution (VarTerm' j) s (AppTerm' t1 t2)           = AppTerm' (substitution (VarTerm' j) s t1) (substitution (VarTerm' j) s t2)
substitution _            _ TrueTerm'                  = TrueTerm'
substitution _            _ FalseTerm'                 = FalseTerm'
substitution (VarTerm' j) s (IfThenElseTerm' t1 t2 t3) = IfThenElseTerm' (substitution (VarTerm' j) s t1) (substitution (VarTerm' j) s t2) (substitution (VarTerm' j) s t3)
substitution _            _ ZeroTerm'                  = ZeroTerm'
substitution (VarTerm' j) s (SuccTerm' t1)             = SuccTerm' (substitution (VarTerm' j) s t1)
substitution (VarTerm' j) s (PredTerm' t1)             = PredTerm' (substitution (VarTerm' j) s t1)
substitution (VarTerm' j) s (IsZeroTerm' t1)           = IsZeroTerm' (substitution (VarTerm' j) s t1)

-- Testing ------------------------------------------------------------------------------------------------------------------------------------------------------
context = [("a", NameBind), ("b", NameBind)]
program1 = AppTerm (VarTerm "b") (AbsTerm "x" NatType (AbsTerm "y" NatType (VarTerm "b")))
x1 = restorenames context $ substitution (removenames context (VarTerm "b")) (removenames context (VarTerm "a")) $ removenames context program1

program2 = AppTerm (VarTerm "b") (AbsTerm "x" NatType (VarTerm "b"))
x2 = restorenames context $ substitution (removenames context $ VarTerm "b") (removenames context $ AppTerm (VarTerm "a") (AbsTerm "z" NatType  (VarTerm "a"))) (removenames context program2)

program3 = AbsTerm "b" NatType (AppTerm (VarTerm "b") (VarTerm "a"))
x3 = restorenames context $ substitution (removenames context $ (VarTerm "b")) (removenames context $ (VarTerm "a")) (removenames context program3)

program4 = AbsTerm "a" NatType (AppTerm (VarTerm "b") (VarTerm "a"))
x4 = restorenames context $ substitution (removenames context $ (VarTerm "b")) (removenames context $ (VarTerm "a")) (removenames context program3)
