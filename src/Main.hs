module Main where

import Control.Monad
import Control.Monad.Writer
import System.IO
import System.Exit

import Lexer
import Parser
import Scope
import Compiler

compileCode c = do
    let lexemes = lexer 1 c
    let tree = parsePScript lexemes
    let ((((_, code), header), sheader), errors) =
         runWriter $ runWriterT $ runWriterT $ runWriterT $ compile tree
    forM_ sheader putStr
    forM_ header putStr
    forM_ code putStr
    errs <- forM errors $ \e -> case e of
        ErrorMsg EErr place msg -> hPutStrLn stderr ("[" ++ place ++ "] error: " ++ msg)
            >> return 1
        ErrorMsg EWarn place msg -> hPutStrLn stderr ("[" ++ place ++ "] warning: " ++ msg)
            >> return 0
        ErrorMsg ENote place msg -> hPutStrLn stderr ("[" ++ place ++ "] note: " ++ msg)
            >> return 0
    putStrLn ""
    when (sum errs > 0) exitFailure

main = do c <- getContents
          compileCode c