module MyLispParser where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

import MyLispCommon

integerParser = do
    s <- option "" (string "-")
    i <- many1 digit
    return $ Integer (read (s ++ i))

identifierSymbol = oneOf "!$%&|*+-/:<=>?@^_~"

identifierParser = do
    s <- letter <|> identifierSymbol
    l <- many $ alphaNum <|> identifierSymbol
    return $ Symbol (s:l)

boolParser = char '#' >> (falseParser <|> trueParser) where
    falseParser = do { char 'f'; return $ Bool False }
    trueParser = do { char 't'; return $ Bool True }

stringParser = do
    char '"'
    s <- many $ noneOf "\""
    char '"'
    return $ String s

quoteParser = do
    char '\''
    e <- expressionParser
    return $ List [Symbol "quote", e]

spaces1 = skipMany1 space

listParser = do { l <- sepEndBy expressionParser spaces1; return $ List l }

dottedListParser = do
    h <- endBy expressionParser spaces1
    t <- char '.' >> spaces1 >> expressionParser
    return $ DottedList h t

expressionParser = spaces >>
    ((try integerParser) <|>
    identifierParser <|>
    boolParser <|>
    stringParser <|>
    quoteParser <|>
    do {
        char '(';
        l <- (try dottedListParser) <|> listParser;
        spaces;
        char ')';
        return l;
    })

genericParser :: Parser a -> String -> Execution a
genericParser parser input = case parse parser "" input of
    Left e -> throwError $ ParserError e
    Right v -> return v

lispParser = genericParser expressionParser

commentsParser = do { char ';'; manyTill anyChar (try newline); }

lispManyParser = genericParser (sepEndBy expressionParser
    (spaces >> skipMany commentsParser))
