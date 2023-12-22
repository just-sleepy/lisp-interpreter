module Evaluator where

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = Atom "Unknown expression"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinop op params = case map unpackNum params of
                            [x, y] -> Bool $ op x y
                            _      -> Bool False

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
boolBoolBinop op params = case map unpackBool params of
                            [x, y] -> Bool $ op x y
                            _      -> Bool False

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _ = False
