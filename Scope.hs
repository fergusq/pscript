module Scope where

import Datatype
import Parser
import Util
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Data.Maybe
import qualified Data.Map as Map

data ErrorSeverity = ENote | EWarn | EErr
data ErrorMsg = ErrorMsg ErrorSeverity String String

type Generator = WriterT [String] (WriterT [String] (WriterT [String] (Writer [ErrorMsg])))
type Compiler a = StateT Scope Generator a

type Subs = Map.Map String PDatatype

data Scope = Scope {
    varscope :: VarScope,
    functions :: Map.Map String Function,
    extends :: Map.Map String [Extend],
    models :: Map.Map String Model,
    structs :: Map.Map String Struct,
    enums :: Map.Map String EnumStruct,
    counter :: Int,
    definedStructs :: [String],
    definedMethods :: [String],
    definedExtends :: [String],
    generatorQueue :: [Compiler ()],
    declarationQueue :: [(Subs, Declaration)]
}

data VarScope = VarScope {
    functionName :: String,
    variables :: Map.Map String PDatatype,
    expectedReturnType :: PDatatype,
    subs :: Subs,
    doesReturn :: Bool
}

putVar :: String -> PDatatype -> Compiler ()
putVar name dtype = do scope <- get
                       let vscope = varscope scope
                           m = variables vscope
                           m' = Map.insert name dtype m
                       put scope { varscope = vscope { variables = m' } }

getVar :: String -> Compiler (Maybe PDatatype)
getVar name = do scope <- get
                 return $ Map.lookup name $ variables $ varscope scope

getVarOrError :: String -> Compiler PDatatype
getVarOrError name = do t <- getVar name
                        case t of
                            Just dt -> return dt
                            Nothing -> do tellError ("Variable not found: " ++ name)
                                          return PNothing

saveScope :: Compiler VarScope
saveScope = do scope <- get
               return $ varscope scope

restoreScope :: VarScope -> Compiler ()
restoreScope vscope = do scope <- get
                         put scope { varscope = vscope }

getExpectedReturnType :: Compiler PDatatype
getExpectedReturnType = do scope <- get
                           return $ expectedReturnType $ varscope scope

getCurrentFunctionName :: Compiler String
getCurrentFunctionName = do scope <- get
                            return $ functionName $ varscope scope

doesThisPathReturn :: Compiler Bool
doesThisPathReturn = get >>= \scope -> return $ doesReturn $ varscope scope

thisPathReturns :: Compiler ()
thisPathReturns = do
    scope@Scope { varscope = vs } <- get
    put scope { varscope = vs { doesReturn = True }}

getFunction :: String -> Compiler (Maybe Function)
getFunction name = do
    scope <- get
    return $ Map.lookup name $ functions scope

getSubstitutions :: PDatatype -> Compiler Subs
getSubstitutions dt@(PInterface n ts) = do
    scope <- get
    let m = models scope
    case Map.lookup n m of
        Just model ->
            let tps = typeparameters model
            in return $ Map.fromList (zip tps ts)
        Nothing -> do
            let s = structs scope
            case Map.lookup n s of
                Just struct ->
                    let tps = stcTypeparameters struct
                    in return $ Map.fromList (zip tps ts)
                Nothing -> do
                    let e = enums scope
                    case Map.lookup n e of
                        Just enum ->
                            let tps = enmTypeparameters enum
                            in return $ Map.fromList (zip tps ts)
                        Nothing ->
                            return Map.empty

getCurrentSubs :: Compiler Subs
getCurrentSubs = do
    Scope { varscope = VarScope { subs = ss } } <- get
    return ss

getModelMethods :: PDatatype -> Compiler [FSignature]
getModelMethods dt@(PInterface a _)
    = do scope <- get
         ss <- getSubstitutions dt
         case Map.lookup a $ models scope of
            Just m -> mapM (subsFunction ss) $ methods m
            Nothing -> return []

getModelMethods dt@(PSum dts)
    = concat <$> mapM getModelMethods dts

getModelMethods PNothing = return [] -- virhe on annettu jo aiemmin

getModelMethods dt = do
    tellError $ show dt ++ " does not have methods"
    return []

getPrerequisites :: PDatatype -> Compiler [PDatatype]
getPrerequisites dt@(PInterface a _) = do
    scope <- get
    ss <- getSubstitutions dt
    case Map.lookup a $ models scope of
            Just m -> mapM (substitute ss) $ prerequisites m
            Nothing -> return []

getExtends :: PDatatype -> Compiler [Extend]
getExtends dt@(PInterface a ts)
    = do scope <- get
         let es = fromMaybe [] (Map.lookup a $ extends scope)
         return es

getExtends dt@(PSum dts)
    = concat <$> mapM getExtends dts

getExtends PNothing = return [] -- virhe on annettu jo aiemmin

getExtends dt = do
    tellError $ show dt ++ " does not have extensions"
    return []

getSubstitutedExtends :: PDatatype -> Compiler [PDatatype]
getSubstitutedExtends dt@(PInterface _ ts) = do
    es <- getExtends dt
    forM es (\e -> do
        let ss = Map.fromList $ zip (eTypeparameters e) ts
        substitute ss $ model e
     )

getSubstitutedExtends PNothing = return []

getSubstitutedExtends dt = do
    tellError $ show dt ++ " does not have extensions"
    return []

getDTypeMethods :: PDatatype -> Compiler [(Extend, FSignature)]
getDTypeMethods dt@(PInterface _ ts)
    = do es <- getExtends dt
         ms <- forM es $ \e -> do
             let ss = Map.fromList $ zip (eTypeparameters e) ts
             t <- substitute ss $ model e
             mms <- getModelMethods t
             forM mms $ \m ->
                return (e, m)
         return $ concat ms

getDTypeMethods PNothing = return [] -- virhe on annettu jo aiemmin

getDTypeMethods dt = do
    tellError $ show dt ++ " does not have methods"
    return []

getModelMethod :: PDatatype -> String -> Compiler (Maybe FSignature)
getModelMethod dt m
    = do ms <- getModelMethods dt
         return $ search sname ms m

getDTypeMethod :: PDatatype -> String -> Compiler (Maybe (Extend, FSignature))
getDTypeMethod dt m
    = do ms <- getDTypeMethods dt
         return $ search (sname.snd) ms m

getFields :: PDatatype -> Compiler (Maybe [(String, PDatatype)])
getFields dt@(PInterface n _) = do
    ss <- getSubstitutions dt
    scope <- get
    let s = Map.lookup n (structs scope)
    case s of
        Just s' -> do
            a <- forM (stcFields s') $ \(n, t) -> do
                t' <- substitute ss t
                return (n, t')
            return $ Just a
        Nothing -> return Nothing

isConstant :: PDatatype -> Compiler (Maybe Bool)
isConstant dt@(PInterface n _) = do
    scope <- get
    let s = Map.lookup n (structs scope)
    return $ do
        s' <- s
        return $ isConst s'

getCases :: PDatatype -> Compiler (Maybe [(String, [PDatatype])])
getCases dt@(PInterface n _) = do
    ss <- getSubstitutions dt
    scope <- get
    let e = Map.lookup n (enums scope)
    case e of
        Just e' -> do
            a <- forM (enmCases e') $ \(n, ts) -> do
                ts' <- forM ts $ \t' ->
                    substitute ss t'
                return (n, ts')
            return $ Just a
        Nothing -> return Nothing

getCases PNothing = return Nothing

getCases dt = do
    tellError $ show dt ++ " does not have enum cases"
    return Nothing

nextNum :: Compiler Int
nextNum = do scope <- get
             let num = counter scope
             put scope { counter = num+1 }
             return num

tmpVar :: Compiler String
tmpVar = do id <- nextNum
            return ("_tmp" ++ show id)

conditionallyCreateStruct :: String -> Compiler () -> Compiler ()
conditionallyCreateStruct n callback = do
    scope <- get
    unless (n `elem` definedStructs scope) $ do
        put scope { definedStructs = n : definedStructs scope }
        callback

conditionallyCreateMethod :: String -> Compiler () -> Compiler ()
conditionallyCreateMethod n callback = do
    scope <- get
    unless (n `elem` definedMethods scope) $ do
        put scope { definedMethods = n : definedMethods scope }
        callback

conditionallyCreateExtend :: String -> Compiler () -> Compiler ()
conditionallyCreateExtend n callback = do
    scope <- get
    unless (n `elem` definedExtends scope) $ do
        put scope { definedExtends = n : definedExtends scope }
        callback

tellError :: String -> Compiler ()
tellError msg = do fname <- getCurrentFunctionName
                   lift . lift . lift . lift $ tell [ErrorMsg EErr ("in " ++ fname) msg]

tellWarning :: String -> Compiler ()
tellWarning msg = do fname <- getCurrentFunctionName
                     lift . lift . lift . lift $ tell [ErrorMsg EWarn ("in " ++ fname) msg]

tellNote :: String -> Compiler ()
tellNote msg = do fname <- getCurrentFunctionName
                  lift . lift . lift . lift $ tell [ErrorMsg ENote ("in " ++ fname) msg]

generateLater :: Compiler () -> Compiler ()
generateLater code = do
    scope <- get
    put scope { generatorQueue = code : generatorQueue scope }

queueDecl :: Subs -> Declaration -> Compiler ()
queueDecl ss d = do
    scope <- get
    put scope { declarationQueue = (ss, d) : declarationQueue scope }

data FSignature = FSignature {
    sname :: String,
    sparameters :: [(String, PDatatype)],
    sreturnType :: PDatatype
}

-- Korvaa tyyppiparametrit tyyppiargumentoilla

substitute :: Subs -> Datatype -> Compiler PDatatype
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

subsFunction :: Subs -> Function -> Compiler FSignature
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
