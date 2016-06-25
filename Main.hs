module Main where
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import Parser
import Lexer

type PVariable = (String, PDatatype)

data PDatatype = PList PDatatype
               | PFunction PDatatype [PDatatype]
               | PReference String
               | PInteger | PBool | PString | PVoid
               | PNothing deriving (Eq, Show)

-- PNothing on pseudotyyppi, jonka kääntäjä antaa virheellisille lausekkeille

-- Tekee jokaisesta deklaraatiosta PVariable-olion

decl2pvar :: Declaration -> PVariable
decl2pvar f = (name f, PFunction (dt2pdt $ returnType f) (map (dt2pdt . snd) $ parameters f))

-- Muuttaa tietotyypin PDatatype-olioksi

dt2pdt :: Datatype -> PDatatype
dt2pdt (Typename "List" [dt]) = PList $ dt2pdt dt
dt2pdt (Typename "Func" (dt:dts)) = PFunction (dt2pdt dt) (map dt2pdt dts)
dt2pdt (Typename "Int" []) = PInteger
dt2pdt (Typename "Str" []) = PString
dt2pdt (Typename "Bool" []) = PBool
dt2pdt (Typename "Void" []) = PVoid
dt2pdt (Typename name _) = PReference name

ctype :: PDatatype -> String -> String
ctype (PList a) n = ctype a ('*':n)
ctype (PFunction r ps) n = ctype r ("(*" ++ n ++ ")(" ++ cparams ps ++ ")")
ctype PInteger n = "int " ++ n
ctype PBool n = "int " ++ n
ctype PString n = "char*" ++ n
ctype PVoid n = "char " ++ n
ctype PNothing n = "void " ++ n
ctype (PReference a) n = "struct " ++ a ++ ('*':n)

cparams :: [PDatatype] -> String
cparams [] = ""
cparams [a] = ctype a ""
cparams (a:as) = ctype a "" ++ ", " ++ cparams as

-- Validaattori käyttää nykyisen muuttujaympäristön määrittelemiseen Map-oliota
-- Mahdolliset virheet kirjoitetaan String-Writeriin

type Generator = WriterT [String] (WriterT [String] (Writer [String]))
type Compiler a = StateT Scope Generator a

data Scope = Scope {
    functionName :: String,
    variables :: Map.Map String PDatatype,
    expectedReturnType :: PDatatype,
    counter :: Int
}

putVar :: String -> PDatatype -> Compiler ()
putVar name dtype = do scope <- get
                       let m = variables scope
                           m' = Map.insert name dtype m
                       put scope { variables = m' }

getVar :: String -> Compiler (Maybe PDatatype)
getVar name = do scope <- get
                 return $ Map.lookup name $ variables scope

getVarOrError :: String -> Compiler PDatatype
getVarOrError name = do t <- getVar name
                        case t of
                            Just dt -> return dt
                            Nothing -> do tellError ("Variable not found: " ++ name)
                                          return PNothing

getExpectedReturnType :: Compiler PDatatype
getExpectedReturnType = do scope <- get
                           return $ expectedReturnType scope

getCurrentFunctionName :: Compiler String
getCurrentFunctionName = do scope <- get
                            return $ functionName scope

nextNum :: Compiler Int
nextNum = do scope <- get
             let num = counter scope
             put scope { counter = num+1 }
             return num

tmpVar :: Compiler String
tmpVar = do id <- nextNum
            return ("tmp" ++ show id)

-- Koodingeneroiminen

generateFunction :: PDatatype -> String -> [(String, PDatatype)] -> Generator ()
generateFunction rtype name params
    = do let paramS = joinColon $ map (\(n,t) -> ctype t n) params
             name' = name ++ "(" ++ paramS ++ ")"
         tell [ctype rtype name', "{\n"]

joinColon :: [String] -> String
joinColon [] = ""
joinColon [p] = p
joinColon (p:ps) = p ++ (',':joinColon ps)

generateIf :: String -> Generator()
generateIf cond
    = tell ["if(", cond, "){\n"]

generateWhile :: String -> Generator()
generateWhile cond
    = tell ["while(", cond, "){\n"]

generateElse :: Generator()
generateElse
    = tell ["}else{\n"]

generateEnd :: Generator()
generateEnd
    = tell ["}\n"]

generateCreate :: PDatatype -> String -> String -> Generator()
generateCreate dt var value
    = tell [ctype dt var,"=",value,";\n"]

generateAssign :: String -> String -> Generator()
generateAssign var value
    = tell [var,"=",value,";\n"]

generateReturn :: String -> Generator()
generateReturn value
    = tell ["return ", value, ";\n"]

generateCode :: String -> Generator()
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

-- Apufunktioita tyyppien tarkistamiseen ja virheisiin

tellError :: String -> Compiler ()
tellError msg = do fname <- getCurrentFunctionName
                   lift . lift . lift $ tell ["Error in " ++ fname ++ ": " ++ msg]

typemismatch :: PDatatype -> PDatatype -> Compiler ()
typemismatch right wrong
    -- PNothing on virheellisten lausekkeiden tyyppi -> virhe on jo tulostettu
    = unless (right == PNothing || wrong == PNothing)
        (tellError ("type mismatch: expected " ++ show right ++ ", got " ++ show wrong))

checktype :: PDatatype -> PDatatype -> Compiler ()
checktype right cand = unless (cand == right) (typemismatch right cand)

checkargs :: [PDatatype] -> [Expression] -> Compiler [String]
checkargs paramTs args = if length paramTs /= length args
                            then do tellError ("wrong number of arguments: "
                                              ++ show (length paramTs) ++ " required, got "
                                              ++ show (length args))
                                    return []
                            else forM (zip paramTs args)
                                  (\(t,v) -> do var <- tmpVar
                                                vt <- compileExpression var v
                                                checktype t vt
                                                return var)

-- Pääfunktio

compile :: [Declaration] -> Generator ()
compile decls = do let pvars = map decl2pvar decls
                   mapM_ (compileDecl pvars) decls

-- Kääntää yksittäisen funktion

compileDecl :: [PVariable] -> Declaration -> Generator ()
compileDecl pvars decl@Function { name = fname, parameters = params,
                                  returnType=rtype, body = Extern }
    = do generateExternFunctionHeader (dt2pdt rtype) [] fname
         return ()
compileDecl pvars decl =
    let fname = name decl
        params = map (\(n,d) -> (n,dt2pdt d)) (parameters decl)
        rtype = dt2pdt $ returnType decl
        scope = Scope {
            functionName = fname,
            variables = Map.fromList $ pvars ++ params,
            expectedReturnType = rtype,
            counter = 0
        }
    in do generateFunctionHeader rtype (map snd params) fname
          generateFunction rtype fname params
          runStateT (compileStatement $ body decl) scope
          generateEnd
          return ()

-- Lauseiden kääntäjät

compileStatement :: Statement -> Compiler ()
compileStatement (Block stmts)
    = do scope <- get
         mapM_ compileStatement stmts
         put scope
compileStatement (Create name expr)
    = do dt <- compileExpression name expr
         putVar name dt
compileStatement (Assign name expr)
    = do var <- tmpVar
         vt <- compileExpression var expr
         lift $ generateAssign name var
         dt <- getVarOrError name
         checktype dt vt
compileStatement (If expr thenBody elseBody)
    = do var <- tmpVar
         dt <- compileExpression var expr
         unless (dt == PBool) (typemismatch PBool dt)
         lift $ generateIf var
         compileStatement thenBody
         case elseBody of
            Just stmt -> do lift generateElse
                            compileStatement stmt
            Nothing -> return ()
         lift generateEnd
compileStatement (For name expr body)
    = do var <- tmpVar
         dt <- compileExpression var expr
         ctr <- tmpVar
         lift $ generateCreate PInteger ctr "0"
         lift $ generateWhile (ctr++"< *"++var)
         case dt of
            PList dt' -> do scope <- get
                            putVar name dt'
                            lift $ generateCreate dt' name (var++"["++ctr++"+1]")
                            compileStatement body
                            lift $ generateAssign ctr (ctr ++ "+1")
                            put scope
            _         -> typemismatch (PList PNothing) dt
         lift generateEnd
compileStatement (Expr (Call expr args))
    = do var <- tmpVar
         dt <- compileExpression var expr
         case dt of
            PFunction retType paramTypes
                -> do argcodes <- checkargs paramTypes args
                      lift $ generateCode (var++"("++joinColon argcodes++");\n")
                      return ()
            _ -> do typemismatch (PFunction PNothing []) dt
                    return ()
compileStatement (Expr expr)
    = do var <- tmpVar
         compileExpression var expr
         return ()
compileStatement (Return expr)
    = do var <- tmpVar
         dt <- compileExpression var expr
         rt <- getExpectedReturnType
         checktype rt dt
         lift $ generateReturn var

-- Lausekkeiden validaattorit

compileExpression :: String -> Expression -> Compiler PDatatype
compileExpression v (Int i) = do lift $ generateCreate PInteger v (show i)
                                 return PInteger
compileExpression v (Str s) = do lift $ generateCreate PString v (show s)
                                 return PString
compileExpression v (Var name)
    = do dt <- getVarOrError name
         lift $ generateCreate dt v name
         return dt
compileExpression v (Call expr args)
    = do var <- tmpVar
         dt <- compileExpression var expr
         case dt of
            PFunction retType paramTypes
                -> do argcodes <- checkargs paramTypes args
                      lift $ generateCreate retType v (var++"("++joinColon argcodes++")")
                      return retType
            _ -> do typemismatch (PFunction PNothing []) dt
                    return PNothing
compileExpression v (List (expr:exprs))
    = do var <- tmpVar
         dt <- compileExpression var expr -- Käännetään ensimmäisen alkion koodi
                                          -- tyypin selvittämiseksi
         lift $ generateCreate (PList dt) v ("malloc("++show (length exprs + 1)
                                             ++ "*sizeof(" ++ ctype dt ""++ "))")
         lift $ generateAssign (v++"[0]") (show $ length exprs + 1)
         lift $ generateAssign (v++"[1]") var
         mapM_ (\(i,val) -> do var' <- tmpVar
                               vt <- compileExpression var' val
                               checktype dt vt
                               lift $ generateAssign (v++"["++show (i+1)++"]") var'
               ) (zip [1..length exprs] exprs)
         return (PList dt)
compileExpression v (MethodCall obj method args)
    = do var <- tmpVar
         dt <- compileExpression var obj
         compileMethodCall v var dt method args

-- Sisäänrakennettujen tyyppien metodit

compileMethodCall :: String -> String
                     -> PDatatype -> String -> [Expression]
                     -> Compiler PDatatype

-- PString
compileMethodCall v obj PString "+" args
    = do checkargs [PString] args
         return PString

-- PInteger
compileMethodCall v obj PInteger method args
    | method == "+" || method == "-" || method == "*" || method == "/"
        = do (var:vars) <- checkargs [PInteger] args
             lift $ generateCreate PInteger v (obj++method++var)
             return PInteger
    | method == "==" || method == "!=" || method == "<" || method == ">"
        || method == "<=" || method == ">="
        = do (var:vars) <- checkargs [PInteger] args
             lift $ generateCreate PInteger v (obj++method++var)
             return PBool

-- PList
compileMethodCall v obj (PList dt) method args
    | method == "[]"
        = do (index:vars) <- checkargs [PInteger] args
             lift $ generateCreate dt v (obj++"["++index++"+1]")
             return dt
    | method == "[]="
        = do (index:value:vars) <- checkargs [PInteger, dt] args
             lift $ generateAssign (obj++"["++index++"+1]") value
             lift $ generateCreate dt v value
             return dt
-- Määrittelemättömät metodit
compileMethodCall v obj dt method args
    = do tellError ("type " ++ show dt ++ " does not have method `" ++ method ++ "'")
         return PNothing

-- Ajaa kääntäjän ja tulostaa virheet

main = do c <- getContents
          let lexemes = lexer c
          let tree = parsePScript lexemes
          let (((_, code), header), errors) =
                runWriter $ runWriterT $ runWriterT $ compile tree
          forM_ header putStr
          forM_ code putStr
          forM_ errors putStrLn
          putStrLn ""
