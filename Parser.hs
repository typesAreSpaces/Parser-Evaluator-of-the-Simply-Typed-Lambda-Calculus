module Parser where

import Data.Char
import Syntax
import Typing
import TypingContext

data Token = TokenArr
    | TokenBool
    | TokenNat
    | TokenVar String
    | TokenAbs 
    | TokenColon
    | TokenDot 
    | TokenApp
    | TokenComma
    | TokenTrue 
    | TokenFalse 
    | TokenIf 
    | TokenFi 
    | TokenThen 
    | TokenElse 
    | TokenZero
    | TokenSucc
    | TokenPred
    | TokenIsZero
    | TokenLParen 
    | TokenRParen deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize cs = tokenizeInner [] cs

tokenizeInner :: [Token] -> String -> [Token]
tokenizeInner tokens [] = reverse tokens
tokenizeInner tokens (c:cs) | c == '(' = tokenizeInner (TokenLParen:tokens) cs
                            | c == ')' = tokenizeInner (TokenRParen:tokens) cs
                            | c == ':' = tokenizeInner (TokenColon:tokens) cs
                            | c == '.' = tokenizeInner (TokenDot:tokens) cs
                            | c == ',' = tokenizeInner (TokenComma:tokens) cs
                            | c == '0' = tokenizeInner (TokenZero:tokens) cs
                            | isSpace c = tokenizeInner tokens cs
                            | isAlpha c = let alphas = takeWhile isAlpha (c:cs)
                                              rest = dropWhile isAlpha (c:cs)
                                          in  tokenizeInner ((kwLookup alphas):tokens) rest 
                            | otherwise = error ("unrecognized character " ++ show c)

kwLookup :: String -> Token
kwLookup "Arr"    = TokenArr
kwLookup "Nat"    = TokenNat
kwLookup "Bool"   = TokenBool
kwLookup "abs"    = TokenAbs
kwLookup "app"    = TokenApp
kwLookup "true"   = TokenTrue
kwLookup "false"  = TokenFalse
kwLookup "if"     = TokenIf
kwLookup "fi"     = TokenFi
kwLookup "then"   = TokenThen
kwLookup "else"   = TokenElse
kwLookup "succ"   = TokenSucc
kwLookup "pred"   = TokenPred
kwLookup "iszero" = TokenIsZero
kwLookup x        = TokenVar x

parseType :: [Token] -> Maybe Type
parseType tokens = case parseInnerType tokens of Just (e, []) -> Just e
                                                 _ -> Nothing

parseInnerType :: [Token] -> Maybe (Type, [Token])
parseInnerType (TokenBool:tokens) = Just (BoolType, tokens)
parseInnerType (TokenNat:tokens)  = Just (NatType, tokens)
parseInnerType (TokenArr:tokens)  = case tokens of
                                    TokenLParen:tokenstail ->
                                        let Just (e', tokens') = parseInnerType tokenstail
                                        in case tokens' of
                                            TokenComma:tokenstail' -> 
                                                let Just (e'', tokens'') = parseInnerType tokenstail'
                                                in case tokens'' of
                                                    TokenRParen:tokenstail'' -> Just (ArrType e' e'', tokenstail'')                                            
                                                    _ -> Nothing
                                            _ -> Nothing
                                    _ -> Nothing
parseInnerType _                  = Nothing

parseTerm :: [Token] -> Maybe Term
parseTerm tokens = case parseInnerTerm tokens of Just (e,[]) -> Just e
                                                 _  -> Nothing

parseInnerTerm :: [Token] -> Maybe (Term, [Token])
parseInnerTerm (TokenTrue:tokens)   = Just (TrueTerm,tokens)
parseInnerTerm (TokenFalse:tokens)  = Just (FalseTerm,tokens)
parseInnerTerm (TokenVar x:tokens)  = Just (VarTerm x,tokens)
parseInnerTerm (TokenAbs:tokens)    = case tokens of
                                        TokenLParen:TokenVar x:TokenColon:tokenstail -> 
                                            let Just (e', tokens') = parseInnerType tokenstail
                                            in case tokens' of
                                                TokenDot:tokenstail'' ->
                                                    let Just (e'', tokens'') = parseInnerTerm tokenstail''
                                                    in case tokens'' of
                                                        TokenRParen:tokenstail''' -> Just (AbsTerm x e' e'', tokenstail''')
                                                        _ -> Nothing
                                                _ -> Nothing
                                        _ -> Nothing
parseInnerTerm (TokenApp:tokens)    = case tokens of
                                        TokenLParen:tokenstail -> 
                                            let Just (e', tokens') = parseInnerTerm tokenstail
                                            in case tokens' of
                                                TokenComma:tokenstail' ->
                                                    let Just (e'', tokens'') = parseInnerTerm tokenstail'
                                                    in case tokens'' of
                                                        TokenRParen:tokenstail'' -> Just (AppTerm e' e'', tokenstail'')
                                                        _ -> Nothing
                                                _ -> Nothing
                                        _ -> Nothing
parseInnerTerm (TokenIf:tokens)     = let Just (e',tokens') = parseInnerTerm(tokens)
                                          in case tokens' of
                                                TokenThen:tokenstail' ->
                                                    let Just (e'',tokens'') = parseInnerTerm tokenstail'
                                                    in case tokens'' of
                                                        TokenElse:tokenstail'' ->
                                                            let Just (e''',tokens''') = parseInnerTerm tokenstail''
                                                            in case tokens''' of
                                                                TokenFi:tokenstail''' -> Just ((IfThenElseTerm e' e'' e'''), tokenstail''')
                                                                _ -> Nothing
                                                        _ -> Nothing
                                                _ -> Nothing
parseInnerTerm (TokenZero:tokens)    = Just (ZeroTerm,tokens)
parseInnerTerm (TokenSucc:tokens)   = let Just (e', tokens') = parseInnerTerm tokens
                                      in Just (SuccTerm e', tokens')
parseInnerTerm (TokenPred:tokens)   = let Just (e', tokens') = parseInnerTerm tokens
                                      in Just (PredTerm e', tokens')
parseInnerTerm (TokenIsZero:tokens) = let Just (e', tokens') = parseInnerTerm tokens
                                      in Just (IsZeroTerm e', tokens')
parseInnerTerm (TokenLParen:tokens) = let Just (e',tokens') = parseInnerTerm tokens
                                      in case tokens' of
                                             TokenRParen:tokenstail' -> Just (e', tokenstail')
                                             _ -> Nothing
parseInnerTerm _                    = Nothing
