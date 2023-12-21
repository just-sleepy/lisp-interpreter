module Lib
    ( someFunc
    ) where
import Control.Monad.State (StateT, put, runStateT)

data Term
  = Nil
  | Number Integer  -- e.g. 5, 42
  | Keyword String    -- e.g. x, first, lambda, -
  | (:::) Term Term

data Value
  = VNumber Integer
  | VList [Term]
  | VLam (Value -> Eval Value)

data Environment -- TODO

type Program = [Term]

type Eval a = StateT Environment (Either String) a

defaultEnvironment :: Environment
defaultEnvironment = error "Implement a default environment"

lookupInEnvironment :: String -> Environment -> Maybe Value
lookupInEnvironment = error "Implement a lookup function based on a HashMap"

insertInEnvironment :: String -> Value -> Environment -> Environment
insertInEnvironment = error "Implement an insert function based on a HashMap"

eval :: Term -> Environment -> Eval Value
eval Nil _ = return $ VList []
eval (Number n) _ = return $ VNumber n
eval (Keyword name) environment = return $ case lookupInEnvironment name environment of
  Just value -> value
  Nothing -> error $ "Unknown keyword: " ++ name
eval (Keyword "define" ::: Keyword name ::: expr ::: Nil) environment = do
  value <- eval expr environment
  put $ insertInEnvironment name value environment
  return $ VList []
eval (Keyword "lambda" ::: Keyword name ::: expr ::: Nil) environment = do
  return $ VLam $ \value -> eval expr (insertInEnvironment name value environment)

repl :: Program -> Environment -> Either String Value
repl program environment = foldr evalAndPrint (Right ([], environment)) program
  where
    evalAndPrint :: Term -> Either String ([Value], Environment) -> Either String ([Value], Environment)
    evalAndPrint term (Left left) = Left left
    evalAndPrint term (Right (values, environment')) =
      let value = runStateT (eval term environment') environment'
      in case value of
        Left err -> Left err
        Right (value', environment'') -> Right (value' : values, environment'')

someFunc :: IO ()
someFunc = putStrLn "someFunc"
