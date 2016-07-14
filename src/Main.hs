module Main where

import Control.Monad
import Control.Monad.Writer
import System.IO
import System.Exit
import System.Environment

import Lexer
import Parser
import Scope
import Compiler
import Util

compileCode :: [String] -> IO ()
compileCode files = do
    tree <- flip rec2 files $ \file -> do
        c <- readFile file
        let lexemes = lexer 1 c
        let (ModuleDecl name imports decls) = parsePScript lexemes
        return (map (++".pscript") imports, decls)
    let ((((((_, code0), code1), header0), header1), header2), errors) =
         runWriter $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ compile tree
    forM_ header2 putStr
    forM_ header1 putStr
    forM_ header0 putStr
    forM_ code1 putStr
    forM_ code0 putStr
    errs <- forM errors $ \e -> case e of
        ErrorMsg EErr place msg -> hPutStrLn stderr ("[" ++ place ++ "] error: " ++ msg)
            >> return 1
        ErrorMsg EWarn place msg -> hPutStrLn stderr ("[" ++ place ++ "] warning: " ++ msg)
            >> return 0
        ErrorMsg ENote place msg -> hPutStrLn stderr ("[" ++ place ++ "] note: " ++ msg)
            >> return 0
    putStrLn ""
    when (sum errs > 0) exitFailure

main = do files <- getArgs
          compileCode files
