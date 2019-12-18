{-# LANGUAGE OverloadedStrings #-}

module LispParser where
import Data.Char
import Parser
import LispVal
import qualified Data.Text as T


readExpr :: T.Text -> Either SchemeError LispVal
readExpr = parse parseExpr



parseExpr :: Parser LispVal
parseExpr = parseKeywords
         <|> parseString
         <|> parseNumber
         <|> parseNegNum
         <|> parseList
         <|> parseQuoted
         <|> parseAtom


ws :: Parser Char
ws = anyOf " \t\n"

spaces = many ws

identStart :: Parser Char
identStart = anyOf "-+/*=|&><?" <|> match isAlpha

identLetter :: Parser Char
identLetter = match isAlphaNum <|> anyOf "?+=|&-/"

parseAtom :: Parser LispVal
parseAtom = failWith (Err "atom") $ do
    head <- identStart
    tail <- many identLetter
    return $ Atom $ T.pack (head:tail)

parseString :: Parser LispVal
parseString = do
    symbol "\""
    string <- many $ noneOf "\""
    symbol "\""
    return $ String $ T.pack string

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 (match isDigit)

parseNegNum :: Parser LispVal
parseNegNum = do
    symbol "-"
    d <- many1 $ match isDigit
    return $ Number . negate . read $ d

parseKeywords :: Parser LispVal
parseKeywords = parseNil <|> parseBool

parseNil :: Parser LispVal
parseNil = symbol "Nil" <|> symbol "nil" >> return Nil

parseBool :: Parser LispVal
parseBool = (symbol "#t" >> return (LispVal.Bool True))
         <|>(symbol "#f" >> return (LispVal.Bool False))

parseInnerList :: Parser LispVal
parseInnerList = List <$> separated parseExpr spaces

parseList :: Parser LispVal
parseList = do
    symbol "("
    spaces
    list <- parseInnerList
    symbol ")"
    return list

parseQuoted :: Parser LispVal
parseQuoted = do
    symbol"\'"
    x <- parseExpr
    return $ List [Atom "quote", x]