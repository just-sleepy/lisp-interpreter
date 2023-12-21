import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data SExpr = Nil | Cons SExpr SExpr | Str String
    deriving (Show)

type Parser = Parsec Void String

sexpr :: Parser SExpr
sexpr = choice [pNil, pString, pNumber, pList, pDefMacro, pDef, pFun, pQuote, pCons, pStr]

pNil :: Parser SExpr
pNil = Nil <$ string "()"

pStr :: Parser SExpr
pStr = Str <$> some alphaNumChar

pCons :: Parser SExpr
pCons = do
    _ <- char '('
    x <- sexpr
    xs <- many (space1 *> sexpr)
    _ <- char ')'
    return $ foldr Cons Nil (x:xs)

pNumber :: Parser SExpr
pNumber = Number <$> decimal

pString :: Parser SExpr
pString = Str <$> (char '"' *> manyTill L.charLiteral (char '"'))

pList :: Parser SExpr
pList = List <$> between (char '(') (char ')') (sepBy sexpr space)

pDefMacro :: Parser SExpr
pDefMacro = DefMacro <$> (string "defmacro*" *> space1 *> some alphaNumChar <* space1) <*> sexpr

pDef :: Parser SExpr
pDef = Def <$> (string "def*" *> space1 *> some alphaNumChar <* space1) <*> sexpr

pFun :: Parser SExpr
pFun = do
    _ <- string "fun*" *> space1
    name <- some alphaNumChar <* space1
    args <- between (char '(') (char ')') (sepBy (some alphaNumChar) space) <* space1
    body <- sexpr
    return $ Fun name args body

pQuote :: Parser SExpr
pQuote = Quote <$> (string "quote" *> space1 *> sexpr <|> char '\'' *> sexpr)