{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import LispVal
import qualified Data.Text as T


type ParserResult a = Either SchemeError (a, T.Text)

newtype Parser a
    = Parser { runParser:: T.Text ->  ParserResult a }

instance Functor Parser where

    fmap fn p = Parser (\input ->
        case runParser p input of
            Right (val, rem) -> Right (fn val, rem)
            Left err -> Left err
            )

instance Applicative Parser where
    pure a = Parser \input -> Right (a, input)
    pFn <*> pApp = Parser \input ->
        case runParser pFn input of
            Right (fn, rem) -> runParser (fmap fn pApp) rem
            Left err -> Left err

instance Monad Parser where
    return = pure
    (>>=) p fn = Parser \input ->
        case runParser p input of
            Right (val, rem) -> runParser (fn val) rem
            Left err -> Left err

parse :: Parser a ->T.Text -> Either SchemeError a
parse p t = fst <$> runParser p t

failParser:: SchemeError -> Parser a
failParser err = Parser \input -> Left err

failWith:: SchemeError -> Parser a -> Parser a
failWith e p = Parser \input ->
    case runParser p input of
        Left _ -> Left e
        r@(Right _) -> r 

ele :: Parser Char
ele = Parser \input ->
    case T.unpack input of
        (x:xs) -> Right (x, T.pack xs)
        [] -> Left $ Err "UnexpectedEndOfInput"

noneOf::T.Text -> Parser Char
noneOf t = do
    head <- ele
    let match = T.all (/= head) t
    case match of
            True -> return head
            False -> failParser $ Err "matched blacklist char"

anyOf:: T.Text -> Parser Char
anyOf t = do
    head <- ele
    let match = T.any (==head) t
    case match of
        True -> return head
        False -> failParser $ Err "No character matched input"

(<|>) :: Parser a -> Parser a -> Parser a
pa <|> pb = Parser \input ->
    case runParser pa input of
        r@(Right _) -> r
        _ -> runParser pb input

match :: (Char -> Bool) -> Parser Char
match fn = do
    head <- ele
    if fn head then
        return head
    else
        failParser $ Err "Predicate did not match"

parseOr:: Parser a -> a -> Parser a
parseOr p val = Parser \input ->
    case runParser p input of
        r@(Right _) -> r
        Left _ -> Right (val, input)

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
    head <- p
    tail <- many p
    return (head:tail)
    
symbol :: T.Text -> Parser [Char]
symbol t = sequence $ fmap (\c -> match (== c)) (T.unpack t)


opt :: Parser a -> Parser (Maybe a)
opt p = (Just <$> p) <|> pure Nothing

separated :: Parser a -> Parser b -> Parser [a]
separated pEle pSep = more <|> once
    where
        more = do
            ele <- pEle
            pSep
            tail <- (separated pEle pSep)
            return $ ele : tail
        once = do 
            ele <- pEle 
            (opt pSep)
            return [ele]

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c