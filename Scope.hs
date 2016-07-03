module Scope where

import Datatype
import Parser
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Data.Maybe
import qualified Data.Map as Map

type Generator = WriterT [String] (WriterT [String] (WriterT [String] (Writer [String])))
type Compiler a = StateT Scope Generator a

data Scope = Scope {
    varscope :: VarScope,
    extends :: Map.Map String [PDatatype],
    models :: Map.Map String Model,
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

getSubstitutions :: PDatatype -> Compiler (Map.Map String PDatatype)
getSubstitutions dt@(PInterface n ts) = do
    scope <- get
    let m = models scope
    case Map.lookup n m of
        Just model ->
            let tps = typeparameters model
            in do
             --tellError $ show (Map.fromList (zip tps ts))
             return $ Map.fromList (zip tps ts)
        Nothing -> return Map.empty

getModelMethods :: PDatatype -> Compiler [FSignature]
getModelMethods dt@(PInterface a _)
    = do scope <- get
         ss <- getSubstitutions dt
         case Map.lookup a $ models scope of
            Just m -> mapM (subsFunction ss) $ methods m
            Nothing -> return []

getExtends :: PDatatype -> Compiler [PDatatype]
getExtends (PInterface a ts)
    = do scope <- get
         let ms = fromMaybe [] (Map.lookup a $ extends scope)
         return ms

getDTypeMethods :: PDatatype -> Compiler [FSignature]
getDTypeMethods dt
    = do ms <- getExtends dt
         mms <- mapM getModelMethods ms
         return $ concat mms

searchMethod :: [FSignature] -> String -> Maybe FSignature
searchMethod ms m
    = let fms = filter (\m' -> sname m' == m) ms
      in case fms of
            []    -> Nothing
            (a:_) -> Just a

getModelMethod :: PDatatype -> String -> Compiler (Maybe FSignature)
getModelMethod dt m
    = do ms <- getModelMethods dt
         return $ searchMethod ms m

getDTypeMethod :: PDatatype -> String -> Compiler (Maybe FSignature)
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

conditionallyCreate :: String -> Compiler () -> Compiler ()
conditionallyCreate n callback = do
    scope <- get
    unless (n `elem` definedStructs scope) $ do
        put scope { definedStructs = n : definedStructs scope }
        callback

tellError :: String -> Compiler ()
tellError msg = do fname <- getCurrentFunctionName
                   lift . lift . lift . lift $ tell ["Error in " ++ fname ++ ": " ++ msg]

data FSignature = FSignature {
    sname :: String,
    sparameters :: [(String, PDatatype)],
    sreturnType :: PDatatype
}

-- Korvaa tyyppiparametrit tyyppiargumentoilla

substitute :: Map.Map String PDatatype -> Datatype -> Compiler PDatatype
substitute subs (Typeparam t) = case Map.lookup t subs of
                                    Just dt -> return dt
                                    _       -> do tellError ("Unknown typeparameter "++t
                                                          ++ " of " ++ show subs)
                                                  return PNothing
substitute subs (Typename n ts) = do
    ts' <- (mapM $ substitute subs) ts
    return $ PInterface n ts'
substitute subs (SumType ts) = do
    ts' <- mapM (substitute subs) ts
    return $ PSum ts'
substitute subs t = return $ dt2pdt t

subsFunction :: Map.Map String PDatatype -> Function -> Compiler FSignature
subsFunction subs f = do
    ps <- mapM (\(n, t) -> s t >>= \t' -> return (n, t')) $ parameters f
    rt <- s $ returnType f
    return FSignature {
                        sname = name f,
                        sparameters = ps,
                        sreturnType = rt
                    }
                    where
                    s = substitute subs
