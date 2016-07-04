module Main where

import Control.Monad
import Control.Monad.Writer
import System.IO
import System.Exit

import Lexer
import Parser
import Compiler

compileCode c = do
    let lexemes = lexer 1 c
    let tree = parsePScript lexemes
    let ((((_, code), header), sheader), errors) =
         runWriter $ runWriterT $ runWriterT $ runWriterT $ compile tree
    forM_ sheader putStr
    forM_ header putStr
    forM_ code putStr
    forM_ errors (hPutStrLn stderr)
    putStrLn ""
    when (length errors > 0) exitFailure

main = do c <- getContents
          compileCode c
