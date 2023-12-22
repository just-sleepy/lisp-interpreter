module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Parser (parseExpr)  
import Evaluator (eval)
import System.IO (hFlush, stdout)
import Control.Monad (unless)

-- REPL
repl :: IO ()
repl = do
    putStr "Lisp>>> "
    hFlush stdout
    input <- getLine
    unless (input == "quit") $ do  -- sai do loop se a entrada for "quit"
        putStrLn $ readExpr input
        repl

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Error: " ++ show err
    Right val -> "Result: " ++ show (eval val)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [expr] -> putStrLn $ readExpr expr
        _ -> putStrLn "Usage: my-lisp-interpreter [expression]"
