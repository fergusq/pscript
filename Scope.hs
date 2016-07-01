-----------------------------------------------------------------------------
--
-- Module      :  Scope
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Scope where

import Datatype
import Parser
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type Generator = WriterT [String] (WriterT [String] (WriterT [String] (Writer [String])))
type Compiler a = StateT Scope Generator a

data Scope = Scope {
    varscope :: VarScope,
    extends :: Map.Map String [PDatatype],
    typeMethods :: Map.Map String [Function],
    counter :: Int,
    definedStructs :: [String]
}

data VarScope = VarScope {
    functionName :: String,
    variables :: Map.Map String PDatatype,
    expectedReturnType :: PDatatype
}

putVar :: String -> PDatatype -> Compiler ()
putVar name dtype = do scope <- get
                       let vscope = varscope scope
                           m = variables vscope
                           m' = Map.insert name dtype m
                       put scope { varscope = vscope { variables = m' } }

saveScope :: Compiler VarScope
saveScope = do scope <- get
               return $ varscope scope

restoreScope :: VarScope -> Compiler ()
restoreScope vscope = do scope <- get
                         put scope { varscope = vscope }

getVar :: String -> Compiler (Maybe PDatatype)
getVar name = do scope <- get
                 return $ Map.lookup name $ variables $ varscope scope

getVarOrError :: String -> Compiler PDatatype
getVarOrError name = do t <- getVar name
                        case t of
                            Just dt -> return dt
                            Nothing -> do tellError ("Variable not found: " ++ name)
                                          return PNothing

getExpectedReturnType :: Compiler PDatatype
getExpectedReturnType = do scope <- get
                           return $ expectedReturnType $ varscope scope

getCurrentFunctionName :: Compiler String
getCurrentFunctionName = do scope <- get
                            return $ functionName $ varscope scope

getModelMethods :: PDatatype -> Compiler [Function]
getModelMethods (PInterface a _)
    = do scope <- get
         case Map.lookup a $ typeMethods scope of
            Just ms -> return ms
            Nothing -> return []

getExtends :: PDatatype -> Compiler [PDatatype]
getExtends (PInterface a ts)
    = do scope <- get
         let ms = fromMaybe [] (Map.lookup a $ extends scope)
         return ms

getDTypeMethods :: PDatatype -> Compiler [Function]
getDTypeMethods dt
    = do ms <- getExtends dt
         mms <- mapM getModelMethods ms
         return $ concat mms

searchMethod :: [Function] -> String -> Maybe Function
searchMethod ms m
    = let fms = filter (\m' -> name m' == m) ms
      in case fms of
            []    -> Nothing
            (a:_) -> Just a

getModelMethod :: PDatatype -> String -> Compiler (Maybe Function)
getModelMethod dt m
    = do ms <- getModelMethods dt
         return $ searchMethod ms m

getDTypeMethod :: PDatatype -> String -> Compiler (Maybe Function)
getDTypeMethod dt m
    = do ms <- getDTypeMethods dt
         return $ searchMethod ms m

nextNum :: Compiler Int
nextNum = do scope <- get
             let num = counter scope
             put scope { counter = num+1 }
             return num

tmpVar :: Compiler String
tmpVar = do id <- nextNum
            return ("tmp" ++ show id)

tellError :: String -> Compiler ()
tellError msg = do fname <- getCurrentFunctionName
                   lift . lift . lift . lift $ tell ["Error in " ++ fname ++ ": " ++ msg]
