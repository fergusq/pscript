module Main where
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Parser
import Lexer

type PVariable = (String, PDatatype)

data PDatatype = PInterface String [PDatatype]
               | PNothing deriving (Eq, Show)

-- PNothing on pseudotyyppi, jonka kääntäjä antaa virheellisille lausekkeille

pList a = PInterface "List" [a]
pFunction r ps = PInterface "Func" (r:ps)
pInteger = PInterface "Int" []
pBool = PInterface "Bool" []
pString = PInterface "Str" []
pVoid = PInterface "Void" []

-- Tekee jokaisesta deklaraatiosta PVariable-olion

decl2pvar :: Function -> PVariable
decl2pvar f = (name f, pFunction (dt2pdt $ returnType f) (map (dt2pdt . snd) $ parameters f))

-- Muuttaa tietotyypin PDatatype-olioksi

dt2pdt :: Datatype -> PDatatype
dt2pdt (Typename "List" [dt]) = pList $ dt2pdt dt
dt2pdt (Typename "Func" (dt:dts)) = pFunction (dt2pdt dt) (map dt2pdt dts)
dt2pdt (Typename "Int" []) = pInteger
dt2pdt (Typename "Str" []) = pString
dt2pdt (Typename "Bool" []) = pBool
dt2pdt (Typename "Void" []) = pVoid
dt2pdt (Typename name dts) = PInterface name (map dt2pdt dts)

ctype :: PDatatype -> String -> String
ctype (PInterface "List" [a]) n = "struct List1" ++ pdt2str a ++ " " ++ n
ctype (PInterface "Func" (r:ps)) n = ctype r ("(*" ++ n ++ ")(" ++ cparams ps ++ ")")
ctype (PInterface "Int" []) n = "int " ++ n
ctype (PInterface "Bool" []) n = "int " ++ n
ctype (PInterface "Str" []) n = "char*" ++ n
ctype (PInterface "Void" []) n = "char " ++ n
ctype PNothing n = "void*" ++ n
ctype (PInterface a _) n = "struct " ++ a ++ ('*':n)

cparams :: [PDatatype] -> String
cparams [] = ""
cparams [a] = ctype a ""
cparams (a:as) = ctype a "" ++ ", " ++ cparams as

pdt2str :: PDatatype -> String
pdt2str (PInterface a []) = a
pdt2str (PInterface a as) = a ++ show (length as) ++ concatMap pdt2str as

-- Validaattori käyttää nykyisen muuttujaympäristön määrittelemiseen Map-oliota
-- Mahdolliset virheet kirjoitetaan String-Writeriin

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

-- Koodingeneroiminen

generateFunction :: PDatatype -> String -> [(String, PDatatype)] -> Compiler ()
generateFunction rtype name params
    = do let paramS = joinColon $ map (\(n,t) -> ctype t n) params
             name' = name ++ "(" ++ paramS ++ ")"
         lift $ tell [ctype rtype name', "{\n"]

joinColon :: [String] -> String
joinColon [] = ""
joinColon [p] = p
joinColon (p:ps) = p ++ (',':joinColon ps)

generateIf :: String -> Compiler ()
generateIf cond
    = lift $ tell ["if(", cond, "){\n"]

generateWhile :: String -> Compiler ()
generateWhile cond
    = lift $ tell ["while(", cond, "){\n"]

generateElse :: Compiler  ()
generateElse
    = lift $ tell ["}else{\n"]

generateEnd :: Compiler ()
generateEnd
    = lift $ tell ["}\n"]

generateCreate :: PDatatype -> String -> String -> Compiler ()
generateCreate dt var value
    = do lift $ tell [ctype dt var,"=",value,";\n"]
         case dt of
            PInterface "List" [a]
                -> do
                    let n = "List1" ++ pdt2str a
                    scope <- get
                    unless (n `elem` definedStructs scope) (do
                        put scope { definedStructs = n : definedStructs scope }
                        lift $ generateSuperHeaderCode
                            ("struct "++n++"{int len;" ++ ctype a "*arr" ++ ";};\n")
                     )
            _   -> return ()

generateAssign :: String -> String -> Compiler ()
generateAssign var value
    = tell [var,"=",value,";\n"]

generateReturn :: String -> Compiler ()
generateReturn value
    = tell ["return ", value, ";\n"]

generateCode :: String -> Generator ()
generateCode code
    = tell [code]

generateVarHeader :: PDatatype -> String -> Generator ()
generateVarHeader dt name
    = lift $ tell [ctype dt name ++ ";\n"]

generateFunctionHeader :: PDatatype -> [PDatatype] -> String -> Generator ()
generateFunctionHeader r ps name
    = lift $ tell [ctype r (name ++ "(" ++ cparams ps ++ ")") ++ ";\n"]

generateExternHeader :: PDatatype -> String -> Generator ()
generateExternHeader dt name
    = lift $ tell ["extern " ++ ctype dt name ++ ";\n"]

generateExternFunctionHeader :: PDatatype -> [PDatatype] -> String -> Generator ()
generateExternFunctionHeader r ps name
    = lift $ tell ["extern " ++ ctype r (name ++ "(" ++ cparams ps ++ ")") ++ ";\n"]

generateHeaderCode :: String -> Generator ()
generateHeaderCode code
    = lift $ tell [code]

generateSuperHeaderCode :: String -> Generator ()
generateSuperHeaderCode code
    = lift . lift $ tell [code]

-- Apufunktioita tyyppien tarkistamiseen ja virheisiin

tellError :: String -> Compiler ()
tellError msg = do fname <- getCurrentFunctionName
                   lift . lift . lift . lift $ tell ["Error in " ++ fname ++ ": " ++ msg]

typemismatch :: PDatatype -> PDatatype -> Compiler ()
typemismatch right wrong
    -- PNothing on virheellisten lausekkeiden tyyppi -> virhe on jo tulostettu
    = unless (right == PNothing || wrong == PNothing)
        (tellError ("type mismatch: expected " ++ show right ++ ", got " ++ show wrong))

checktype :: String -> String -> PDatatype -> PDatatype -> Compiler ()

checktype v f right cand
    = do es <- getExtends cand
         if right `elem` es
            then do generateCreate right v ("alloc(sizeof(struct "++pdt2str right++"))")
                    ms <- getModelMethods right
                    forM_ ms (\m ->
                        generateAssign (v++"->"++name m) (pdt2str cand++'_':name m)
                     )
                    generateAssign (v++"->_obj") ("alloc(sizeof("++ctype cand ""++"))")
                    generateAssign ("*("++ctype cand "*"++")"++v++"->_obj") f
            else case (right, cand) of
                (PInterface "List" [t], PInterface "List" [u])
                    -> do generateCreate right v
                            ('{': f ++ ".len, alloc(" ++ f ++ ".len*sizeof(" ++ ctype right "" ++ "))}")
                          generateAssign (v++".len") (f++".len")
                          ctr <- tmpVar
                          generateCreate pInteger ctr "0"
                          generateWhile (ctr++"<"++f++".len")
                          var <- tmpVar
                          checktype var (f++".arr["++ctr++"]") t u
                          generateAssign (v++".arr["++ctr++"]") var
                          generateAssign ctr (ctr ++ "+1")
                          generateEnd
                _
                    -> do unless (cand == right) (typemismatch right cand)
                          generateCreate right v f

checkargs :: [PDatatype] -> [Expression] -> Compiler [String]
checkargs paramTs args = if length paramTs /= length args
                            then do tellError ("wrong number of arguments: "
                                              ++ show (length paramTs) ++ " required, got "
                                              ++ show (length args))
                                    return []
                            else forM (zip paramTs args)
                                  (\(t,v) -> do var <- tmpVar
                                                vt <- compileExpression var v
                                                var' <- tmpVar
                                                checktype var' var t vt
                                                return var')

-- Pääfunktio

compile :: [Declaration] -> Generator ()
compile decls = do let pvars = map decl2pvar
                        $ concatMap (\d -> case d of Func f -> [f]
                                                     _     -> []) decls
                   let models = Map.fromList $
                                concatMap (\d -> case d of Mdl m -> [(modelName m, methods m)]
                                                           _     -> []) decls
                   let extends = concatMap (\d -> case d of Ext e -> [(dtName e, dt2pdt $ model e)]
                                                            _     -> []) decls
                   let supers = collapse extends
                   generateHeaderCode "#include <stdlib.h>\n"
                   generateHeaderCode "void * alloc(size_t x) { return malloc(x); }\n"
                   let scope = Scope {
                      varscope = VarScope {
                        functionName = "",
                        variables = Map.empty,
                        expectedReturnType = PNothing
                      },
                      counter = 0,
                      extends = supers,
                      typeMethods = models,
                      definedStructs = []
                   }
                   runStateT (rec (mapM (compileDecl pvars)) decls) scope
                   return ()

collapse :: (Ord k) => [(k, a)] -> Map.Map k [a]
collapse = foldr (\(k, v) m -> Map.insert k (v : fromMaybe [] (Map.lookup k m)) m) Map.empty

rec :: (Monad m) => ([a] -> m [[a]]) -> [a] -> m()
rec f [] = return ()
rec f a  = do v <- f a
              rec f (concat v)

-- Kääntää yksittäisen funktion

compileDecl :: [PVariable] -> Declaration
               -> Compiler [Declaration]
compileDecl pvars (Func decl@Function { name = fname, parameters = params,
                                        returnType=rtype, body = Extern })
    = do lift $ generateExternFunctionHeader (dt2pdt rtype) [] fname
         return []
compileDecl pvars (Func func) =
    let fname = name func
        params = map (\(n,d) -> (n,dt2pdt d)) (parameters func)
        rtype = dt2pdt $ returnType func
    in do scope <- get
          let vscope = varscope scope
          put scope { varscope = vscope {
              functionName = fname,
              variables = Map.fromList $ pvars ++ params,
              expectedReturnType = rtype
           }}
          lift $ generateFunctionHeader rtype (map snd params) fname
          generateFunction rtype fname params
          compileStatement $ body func
          generateEnd
          return []
compileDecl _ (Mdl m) =
    do lift $ generateHeaderCode ("struct " ++ modelName m ++ "{\n")
       forM_ (methods m) (\f ->
            let r = dt2pdt $ returnType f
                ps = PInterface (modelName m) [] : map (dt2pdt.snd) (parameters f)
            in lift $ generateHeaderCode("    " ++
                ctype r ('(':'*':name f ++ ")(" ++ cparams ps ++ ")") ++ ";\n"))
       lift $ generateHeaderCode "    void *_obj;\n};\n"
       return []
compileDecl _ (Ext Extend { dtName = n, model = m, eMethods = fs}) =
    forM fs (\f -> do
        let r = dt2pdt $ returnType f
            ps = ctype (dt2pdt m) "this" : map (\(n,d) -> ctype (dt2pdt d) n) (parameters f)
            ps' = ("*(("++ctype (PInterface n []) "*" ++")this->_obj)") : map fst (parameters f)
            signature = ctype r (n ++ "_" ++ name f ++ "(" ++ joinColon ps ++ ")")
        lift $ generateHeaderCode (signature ++ ";\n")
        lift $ generateCode (signature ++ " {\n")
        lift $ generateCode ("    _" ++ n ++ "_" ++ name f ++ "(" ++ joinColon ps' ++ ");\n}\n")
        return (Func f {
            name = '_' : n ++ "_" ++ name f,
            parameters = ("this", Typename n []) : parameters f
        })
    )

-- Lauseiden kääntäjät

compileStatement :: Statement -> Compiler ()
compileStatement (Block stmts)
    = do scope <- saveScope
         mapM_ compileStatement stmts
         restoreScope scope
compileStatement (Create name expr)
    = do dt <- compileExpression name expr
         putVar name dt
compileStatement (Assign name expr)
    = do var <- tmpVar
         vt <- compileExpression var expr
         dt <- getVarOrError name
         var' <- tmpVar
         checktype var' var dt vt
         generateAssign name var'
compileStatement (If expr thenBody elseBody)
    = do var <- tmpVar
         dt <- compileExpression var expr
         unless (dt == pBool) (typemismatch pBool dt)
         generateIf var
         compileStatement thenBody
         case elseBody of
            Just stmt -> do generateElse
                            compileStatement stmt
            Nothing -> return ()
         generateEnd
compileStatement (For name expr body)
    = do var <- tmpVar
         dt <- compileExpression var expr
         ctr <- tmpVar
         generateCreate pInteger ctr "0"
         generateWhile (ctr++"<"++var++".len")
         case dt of
            PInterface "List" [dt']
                -> do scope <- saveScope
                      putVar name dt'
                      generateCreate dt' name (var++".arr["++ctr++"]")
                      compileStatement body
                      generateAssign ctr (ctr ++ "+1")
                      restoreScope scope
            _   -> typemismatch (pList PNothing) dt
         generateEnd
compileStatement (Expr (Call expr args))
    = do var <- tmpVar
         dt <- compileExpression var expr
         case dt of
            PInterface "Func" (retType:paramTypes)
                -> do argcodes <- checkargs paramTypes args
                      lift $ generateCode (var++"("++joinColon argcodes++");\n")
                      return ()
            _ -> do typemismatch (pFunction PNothing []) dt
                    return ()
compileStatement (Expr expr)
    = do var <- tmpVar
         compileExpression var expr
         return ()
compileStatement (Return expr)
    = do var <- tmpVar
         dt <- compileExpression var expr
         rt <- getExpectedReturnType
         var' <- tmpVar
         checktype var' var rt dt
         generateReturn var'

-- Lausekkeiden validaattorit

compileExpression :: String -> Expression -> Compiler PDatatype
compileExpression v (Int i) = do generateCreate pInteger v (show i)
                                 return pInteger
compileExpression v (Str s) = do generateCreate pString v (show s)
                                 return pString
compileExpression v (Var name)
    = do dt <- getVarOrError name
         generateCreate dt v name
         return dt
compileExpression v (Call expr args)
    = do var <- tmpVar
         dt <- compileExpression var expr
         case dt of
            PInterface "Func" (retType:paramTypes)
                -> do argcodes <- checkargs paramTypes args
                      generateCreate retType v (var++"("++joinColon argcodes++")")
                      return retType
            _ -> do typemismatch (pFunction PNothing []) dt
                    return PNothing
compileExpression v (List (expr:exprs))
    = do var <- tmpVar
         dt <- compileExpression var expr -- Käännetään ensimmäisen alkion koodi
                                          -- tyypin selvittämiseksi
         generateCreate (pList dt) v ('{': show (length exprs) ++ ", alloc("
                                      ++ show (length exprs)
                                      ++ "*sizeof(" ++ ctype dt "" ++ "))}")
         generateAssign (v++".len") (show $ length exprs + 1)
         generateAssign (v++".arr[0]") var
         mapM_ (\(i,val) -> do var' <- tmpVar
                               vt <- compileExpression var' val
                               var'' <- tmpVar
                               checktype var'' var' dt vt
                               generateAssign (v ++ ".arr["++show i++"]") var''
               ) (zip [1..length exprs] exprs)
         return (pList dt)
compileExpression v (NewList dt size)
    = do let pdt = dt2pdt dt
         var <- tmpVar
         vt <- compileExpression var size
         var' <- tmpVar
         checktype var' var pInteger vt
         generateCreate (pList pdt) v ('{': var' ++ ", alloc("
                                      ++ var'
                                      ++ "*sizeof(" ++ ctype pdt "" ++ "))}")
         return (pList pdt)
compileExpression v (MethodCall obj method args)
    = do var <- tmpVar
         dt <- compileExpression var obj
         compileMethodCall v var dt method args

-- Sisäänrakennettujen tyyppien metodit

type MethodCallCompiler = String -> String
                     -> PDatatype -> String -> [Expression]
                     -> Compiler PDatatype

compileMethodCall :: MethodCallCompiler

-- PString
compileMethodCall v obj (PInterface "Str" []) "+" args
    = do checkargs [pString] args
         return pString

-- PInteger
compileMethodCall v obj (PInterface "Int" []) method args
    | method == "+" || method == "-" || method == "*" || method == "/"
        = do (var:vars) <- checkargs [pInteger] args
             generateCreate pInteger v (obj++method++var)
             return pInteger
    | method == "==" || method == "!=" || method == "<" || method == ">"
        || method == "<=" || method == ">="
        = do (var:vars) <- checkargs [pInteger] args
             generateCreate pInteger v (obj++method++var)
             return pBool

-- PList
compileMethodCall v obj (PInterface "List" [dt]) method args
    | method == "[]"
        = do (index:vars) <- checkargs [pInteger] args
             generateCreate dt v (obj ++ ".arr["++index++"]")
             return dt
    | method == "[]="
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             generateAssign (obj ++ ".arr["++index++"]") value
             generateCreate dt v value
             return dt
-- Määrittelemättömät metodit
compileMethodCall v obj dt method args
    = do m <- getDTypeMethod dt method
         case m of
            Nothing -> do p <- getModelMethod dt method
                          case p of
                            Nothing -> do
                                tellError ("type " ++ show dt ++ " does not have method `" ++ method ++ "'")
                                return PNothing
                            Just p' ->
                                compileMethodCall' v p' (obj++"->"++method) obj args
            Just m' -> compileMethodCall' v m' (pdt2str dt++'_':method) obj args

compileMethodCall' v f n obj args = do
    argcodes <- checkargs (map (\(n,d) -> dt2pdt d) (parameters f)) args
    generateCreate (dt2pdt $ returnType f) v
        (n++"("++joinColon (obj:argcodes)++")")
    return (dt2pdt $ returnType f)

-- Ajaa kääntäjän ja tulostaa virheet

main = do c <- getContents
          let lexemes = lexer c
          let tree = parsePScript lexemes
          let ((((_, code), header), sheader), errors) =
                runWriter $ runWriterT $ runWriterT $ runWriterT $ compile tree
          forM_ sheader putStr
          forM_ header putStr
          forM_ code putStr
          forM_ errors (hPutStrLn stderr)
          putStrLn ""
