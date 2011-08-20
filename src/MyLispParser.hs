module MyLispParser (myLispParser) where

import Control.Monad.Error
import Data.Char
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import MyLispCommon

lexer = T.makeTokenParser $ T.LanguageDef {
    T.commentStart = "#|",
    T.commentEnd = "|#",
    T.nestedComments = True,
    T.commentLine = ";",
    T.identStart = letter <|> identStartSpecialSym,
    T.identLetter = alphaNum <|> identLetterSpecialSym,
    T.opStart = unexpected "internal error: should not parse operators",
    T.opLetter = unexpected "internal error: should not parse operators",
    T.reservedNames = [],
    T.reservedOpNames = [],
    T.caseSensitive = False
} where
    identStartSpecialSym = oneOf "!$%&*/:<=>?^_~"
    identLetterSpecialSym = identStartSpecialSym <|> oneOf "+-.@"

whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
symbol = T.symbol lexer
identifier = T.identifier lexer
stringLiteral = T.stringLiteral lexer
natural = T.natural lexer
float = T.float lexer
parens = T.parens lexer

numberParser :: Parser Expression
numberParser = (try realParser) <|> integerParser where
    signParser :: Num a => Parser (a -> a)
    signParser =
        (char '-' >> return negate) <|>
        (char '+' >> return id) <|>
        return id
    realParser = signParser >>= (\s -> float >>= return . Real . s)
    integerParser = signParser >>= (\s -> natural >>= return . Integer . s)

symbolParser :: Parser Expression
symbolParser = (identifier <|> identSpecial) >>= return . Symbol . map toLower
    where identSpecial = symbol "+" <|> symbol "-" <|> symbol "..."

boolParser :: Parser Expression
boolParser = lexeme $ char '#' >> (falseParser <|> trueParser) where
    falseParser = do { oneOf "fF"; return $ Bool False }
    trueParser = do { oneOf "tT"; return $ Bool True }

charParser :: Parser Expression
charParser = lexeme $ string "#\\" >> anyChar >>= return . Char

stringParser :: Parser Expression
stringParser = stringLiteral >>= return . String

quoteParser :: Parser Expression
quoteParser =
    symbol "'" >> expressionParser >>= (\e -> return $ List [Symbol "quote", e])

listParser :: Parser Expression
listParser = do
    h <- many expressionParser
    if null h
        then return $ List []
        else (symbol "." >> expressionParser >>= return . DottedList h) <|>
             (return $ List h)

expressionParser :: Parser Expression
expressionParser =
    ((parens listParser) <?> "list/dotted list") <|>
    ((try numberParser) <?> "number") <|>
    ((try symbolParser) <?> "symbol") <|>
    (stringParser <?> "string") <|>
    ((try boolParser) <?> "bool") <|>
    (charParser <?> "char") <|>
    (quoteParser <?> "'")

multiExpressionParser :: Parser [Expression]
multiExpressionParser = do
    whiteSpace
    e <- many expressionParser
    eof
    return e

myLispParser :: FilePath -> String -> Execution [Expression]
myLispParser inSrc input = case parse multiExpressionParser inSrc input of
    Left e -> throwError $ ParserError $ show e
    Right v -> if null v then return [Void] else return v