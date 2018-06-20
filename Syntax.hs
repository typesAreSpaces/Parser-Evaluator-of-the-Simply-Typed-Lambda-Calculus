-- Completed
module Syntax where

data Type = ArrType Type Type
    | BoolType
    | NatType deriving Eq

instance Show Type where  
    show BoolType        = "Bool"
    show NatType         = "Nat"
    show (ArrType t1 t2) = "Arr(" ++ show t1 ++ "," ++ show t2 ++ ")"

data Term = VarTerm String
    | AbsTerm String Type Term
    | AppTerm Term Term
    | TrueTerm 
    | FalseTerm
    | IfThenElseTerm Term Term Term
    | ZeroTerm
    | SuccTerm Term
    | PredTerm Term
    | IsZeroTerm Term deriving Eq

instance Show Term where  
    show (VarTerm x)               = x
    show (AbsTerm x ty tm)         = "abs(" ++ x ++ ":" ++ show ty ++ "." ++ show tm ++ ")"
    show (AppTerm t1 t2)           = "app(" ++ show t1 ++ "," ++ show t2 ++ ")"
    show (TrueTerm)                = "true"
    show (FalseTerm)               = "false"
    show (IfThenElseTerm t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
    show (ZeroTerm)                = "0"
    show (SuccTerm t)              = "succ(" ++ show t ++ ")"
    show (PredTerm t)              = "pred(" ++ show t ++ ")"
    show (IsZeroTerm t)            = "iszero(" ++ show t ++ ")"

show2 :: Term -> String
show2 (AbsTerm x _ tm) = "abs(" ++ x ++ "." ++ show tm ++ ")"
show2 x                = show x

data Binding = NameBind
    | VarBind Type deriving (Eq, Show)
