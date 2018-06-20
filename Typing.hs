module Typing where

import Syntax
import TypingContext

typeOf :: TypingContext -> Term -> Maybe Type
typeOf []     (VarTerm x)                  = Nothing                                                -- T-Var
typeOf (y:ys) (VarTerm x)                  = if (x == fst y)                                        -- T-Var
                                                then case (snd y) of
                                                    VarBind ty -> Just ty
                                                    NameBind -> Nothing 
                                                else typeOf ys (VarTerm x)
typeOf xs     (AbsTerm x ty1 tm2)          = let ty2 = typeOf ((x, VarBind ty1):xs) tm2 in          -- T-Abs
                                                case ty2 of 
                                                    Just x -> Just (ArrType ty1 x)
                                                    _ -> Nothing
typeOf ctx    (AppTerm tm1 tm2)            = let ty1 = typeOf ctx tm1 in                            -- T-App
                                                let ty2 = typeOf ctx tm2 in 
                                                    case ty1 of 
                                                        Just (ArrType x y) -> if (Just x == ty2) 
                                                                then Just y 
                                                                else Nothing
                                                        _ -> Nothing
typeOf _      TrueTerm                     = Just BoolType                                          -- T-True
typeOf _      FalseTerm                    = Just BoolType                                          -- T-False
typeOf ctx    (IfThenElseTerm tm1 tm2 tm3) = let ty1 = typeOf ctx tm1 in                            -- T-If
                                                case ty1 of
                                                    Just BoolType -> 
                                                        if (typeOf ctx tm2 == typeOf ctx tm3)
                                                            then typeOf ctx tm2
                                                            else Nothing
                                                    _ -> Nothing
typeOf _      ZeroTerm                     = Just NatType                                           -- T-Zero
typeOf ctx    (SuccTerm x)                 = case (typeOf ctx x) of                                 -- T-Succ
                                                Just NatType -> Just NatType
                                                _ -> Nothing
typeOf ctx    (PredTerm x)                 = case (typeOf ctx x) of                                 -- T-Pred
                                                Just NatType -> Just NatType
                                                _ -> Nothing
typeOf ctx    (IsZeroTerm x)               = case (typeOf ctx x) of                                 -- T-IsZero
                                                Just NatType -> Just BoolType
                                                _ -> Nothing

showType :: Maybe Type -> String
showType (Just x) = show x
showType _        = "TLBN: type error"
