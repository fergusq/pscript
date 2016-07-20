module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import Data.List.Split
import System.IO
import System.Exit
import System.Environment
import System.Directory

import Lexer
import Parser
import Scope
import Compiler
import Documentator
import Util

findModuleFile :: String -> [FilePath] -> IO (Maybe FilePath)
findModuleFile _ [] = return Nothing
findModuleFile moduleName (dir:path) = do
    files <- getDirectoryContents dir
    let name = moduleName ++ ".pscript"
    if name `elem` files then
        return (Just $ dir++"/"++name)
    else
        findModuleFile moduleName path

parseModulesRecursively :: [FilePath] -> [String] -> IO [Declaration]
parseModulesRecursively searchPath files = do
    (tree, _) <- flip runStateT [] $ flip rec2 files $ \file -> do
        imported_modules <- get
        if file `elem` imported_modules
         then return ([], [])
         else do
            put (file : imported_modules)
            c <- lift . readFile $ file
            let lexemes = lexer 1 c
            let (ModuleDecl name imports decls) = parsePScript lexemes
            importFiles <- forM imports $ \moduleName -> do
                maybeFile <- lift $ findModuleFile moduleName searchPath
                case maybeFile of
                    Just filepath -> return filepath
                    Nothing -> do
                        lift $ hPutStrLn stderr ("Module not found: "++moduleName)
                        lift exitFailure
                        return ""
            return (importFiles, decls)
    return tree

compileCode :: [FilePath] -> [String] -> IO ()
compileCode searchPath files = do
    tree <- parseModulesRecursively searchPath files
    let ((((((_, code0), code1), header0), header1), header2), errors) =
         runWriter $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ runWriterT $
            compile tree
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

documentCode searchPath files = do
    tree <- parseModulesRecursively searchPath files
    let (_, document) = runWriter $ documentTree tree
    forM_ document putStrLn

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: psc <command> [args]"
    putStrLn "Available commands:"
    putStrLn "  compile - compiles given pscript files to c"
    putStrLn "  document - generates documentation for given pscript files in c"
    putStrLn "  help - show this help text"
    exitSuccess

type Args = Map.Map String [String]

addArg :: String -> a -> (a -> [String] -> [String]) -> Args -> Args
addArg key value add m =
    Map.insert key newList m
    where
    newList = value `add` oldList
    (Just oldList) = Map.lookup key m

parseArgs :: [String] -> Args -> Args
parseArgs []              = id
parseArgs ("--path":a:as) = parseArgs as . addArg "path" (splitOn ":" a) (++)
parseArgs ("-p":a:as)     = parseArgs as . addArg "path" (splitOn ":" a) (++)
parseArgs (a:as)          = parseArgs as . addArg "files" a (:)

main = do
    appUserDataDir <- getAppUserDataDirectory "pscript"
    auddExists <- doesDirectoryExist appUserDataDir
    let defaultArgs = Map.fromList [
                ("files", []),
                ("path", [appUserDataDir | auddExists])
            ]
    (command:cmdArgs) <- getArgs
    let args = parseArgs cmdArgs defaultArgs
    let (Just files) = Map.lookup "files" args
    let (Just path) = Map.lookup "path" args
    case command of
        "compile" ->
            compileCode path files
        "document" ->
            documentCode path files
        "help" ->
            printHelp
        cmd -> do
            putStrLn ("Unknown command `" ++ cmd ++ "'")
            printHelp
