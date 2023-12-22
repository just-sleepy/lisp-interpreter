module Lib where

parse :: [Char] -> Program
parse = undefined

type Program = [SExp]

data SExp
  = Literal Integer -- e.g. 5, 42
  | Symbol String -- e.g. x, first, lambda, -
  | SExp [SExp] -- e.g. (+ (* 3 2) 4), (define fact (lambda n (fact (- n 1))))

instance Show SExp where
  show (Literal i) = show i
  show (Symbol s) = show s
  show (SExp list) = "(" ++ show list ++ ")"

data Value
  = Num Integer
  | List [SExp]
  | Lam (Value -> Value)

instance Show Value where
  show (List values) = "(" ++ show values ++ ")"
  show (Num i) = show i
  show (Lam _) = "<lambda>"

type SymbolTable = [(String, Value)]

-- Yes, a State monad is applicable here, but it's not really necessary
-- for this exposition.
eval :: SExp -> SymbolTable -> (Value, SymbolTable)
-- If the expression is just an integer, it has no effect on the symbol table
-- but we should print out that integer.
eval (Literal n) table = (Num n, table)
eval (Symbol s) table =
  case lookup s table of
    Just value -> (value, table)
    Nothing -> error ("The value " ++ s ++ " is not defined! Aborting.")
eval (SExp [Symbol "define", Symbol s, expr]) table =
  let value = fst $ eval expr table
   in -- Return the new value and update the symbol table, overwriting the old
      -- definition in the stupidest way possible.
      (value, (s, value) : table)
-- Note that in real Lisps, the expression '(1 2 3) is a *reader shorthand*
-- that expands to (quote 1 2 3).
eval (SExp (Symbol "quote" : exprs)) table = (List exprs, table)
eval (SExp [Symbol "lambda", Symbol s, expr]) table =
  let fn value = fst $ eval expr ((s, value) : table)
   in (Lam fn, table)
-- This is the general case,
eval (SExp (car : cdr)) table =
  let (fn, _) = eval car table
      args = map (fst . (`eval` table)) cdr
   in (applyRecursive fn args, table)
   where -- This will get us Curried function application.
         applyRecursive :: Value -> [Value] -> Value
         applyRecursive = foldr next where
           next (Lam fn) value = fn value
           next not_a_function _  = error ("Cannot apply non-function " ++ show not_a_function)
eval _ _ = error "Fodeu"

binop :: (Integer -> Integer -> Integer) -> Value
binop f = Lam $ \lhs -> Lam $ \rhs -> Num $ f (unwrapInt lhs) (unwrapInt rhs)

standardLibrary :: SymbolTable
standardLibrary = [("+", binop (+)),
                   ("-", binop (-))]

unwrapInt :: Value -> Integer
unwrapInt (Num i) = i
unwrapInt _ = error "Expected integer"

repl :: Program -> [Value]
repl prog = fst $ foldr evalAndPrint ([], standardLibrary) prog
 where
  evalAndPrint :: SExp -> ([Value], SymbolTable) -> ([Value], SymbolTable)
  evalAndPrint expr (values, table) =
    let (newval, newtable) = eval expr table
    in (newval : values, newtable)
