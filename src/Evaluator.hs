module Evaluator where

import Debug.Trace


data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Function ([LispVal] -> LispVal)

instance Show LispVal where
    show (Atom name) = "Atom \"" ++ name ++ "\""
    show (List contents) = "List " ++ show contents
    show (Number contents) = "Number " ++ show contents
    show (String contents) = "String \"" ++ contents ++ "\""
    show (Bool True) = "Bool True"
    show (Bool False) = "Bool False"
    show (Function _) = "<Function>"

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom "filter" : Atom funcName : rest)) = lispFilter (Atom funcName : map eval rest)
eval (List (Atom "map" : Atom funcName : rest)) = lispMap (Atom funcName : map eval rest)
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = Atom "Unknown expression"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Atom $ "Unknown function: " ++ func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("cons", lispCons),
              ("head", lispHead),
              ("tail", lispTail),
              ("double", double),
              ("isEven", isEven),
              ("map", lispMap),
              ("filter", lispFilter),
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

lispCons :: [LispVal] -> LispVal
lispCons (x:List xs:[]) = List (x:xs)
lispCons _ = Atom "cons usage: (cons elem list)"

lispHead :: [LispVal] -> LispVal
lispHead (List (x:_):[]) = x
lispHead _ = Atom "head usage: (head list)"

lispTail :: [LispVal] -> LispVal
lispTail (List (_:xs):[]) = List xs
lispTail _ = Atom "tail usage: (tail list)"

lispMap :: [LispVal] -> LispVal
lispMap (Atom funcName:List xs:[]) =
    case lookup funcName primitives of
        Just func -> List $ map (\x -> func [x]) xs
        Nothing -> Atom $ "Unknown function: " ++ funcName
lispMap _ = Atom "map usage: (map function list)"

lispFilter :: [LispVal] -> LispVal
lispFilter (Atom funcName:List xs:[]) =
    case lookup funcName primitives of
        Just func -> List $ filter (unpackBool . func . return) xs
        Nothing -> Atom $ "Unknown function: " ++ funcName
lispFilter _ = Atom "filter usage: (filter function list)"

double :: [LispVal] -> LispVal
double [Number n] = Number (n * 2)
double _ = Atom "double usage: (double number)"

isEven :: [LispVal] -> LispVal
isEven [Number n] = Bool (n `mod` 2 == 0)
isEven _ = Atom "isEven usage: (isEven number)"

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _ = False
