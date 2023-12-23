module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Evaluator (LispVal(..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf """)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol <|> char '#'  -- Include '#' for boolean literals
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
                 "#t" -> Bool True
                 "#f" -> Bool False
                     -> Atom atom


parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> do char '('
                x <- try parseList
                char ')'
                return x
