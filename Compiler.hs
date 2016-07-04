module Compiler where

import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import System.IO
import Parser
import Lexer
import Util
import Datatype
import Scope

-- Koodingeneroiminen

generateFunction :: PDatatype -> String -> [(String, PDatatype)] -> Compiler ()
generateFunction rtype name params
    = do let paramS = joinColon $ map (\(n,t) -> ctype t n) params
             name' = name ++ "(" ++ paramS ++ ")"
         lift $ tell [ctype rtype name', "{\n"]

generateIf :: String -> Compiler ()
generateIf cond
    = lift $ tell ["    if(", cond, "){\n"]

generateWhile :: String -> Compiler ()
generateWhile cond
    = lift $ tell ["    while(", cond, "){\n"]

generateElse :: Compiler  ()
generateElse
    = lift $ tell ["    }else{\n"]

generateEnd :: Compiler ()
generateEnd
    = lift $ tell ["    }\n"]

generateCreate :: PDatatype -> String -> String -> Compiler ()
generateCreate dt var value
    = do lift $ tell ["    ", ctype dt var,"=",value,";\n"]
         ensureStructIsDefined dt

generateAssign :: String -> String -> Compiler ()
generateAssign var value
    = tell ["    ",var,"=",value,";\n"]

generateReturn :: String -> Compiler ()
generateReturn value
    = tell ["    return ", value, ";\n"]

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

typemismatch :: PDatatype -> PDatatype -> Compiler ()
typemismatch right wrong
    -- PNothing on virheellisten lausekkeiden tyyppi -> virhe on jo tulostettu
    = unless (right == PNothing || wrong == PNothing)
        (tellError ("type mismatch: can't cast " ++ show wrong ++ " to " ++ show right))

checktype :: String -> String -> PDatatype -> PDatatype -> Compiler ()

checktype v f right cand@(PSum dts)
    = do unless (cand == right) (typemismatch right cand)
         generateCreate right v f
checktype v f right@(PSum dts) cand
    = createModelObject v f right dts cand
checktype v f right cand = do
    es <- getExtends cand
    let PInterface _ ts = cand
    ms <- mapM (\e -> do
        let ss = Map.fromList $ zip (eTypeparameters e) ts
        substitute ss $ model e
     ) es
    forM_ es $ ensureExtendIsDefined cand
    if right `elem` ms
        then createModelObject v f right [right] cand
        else case (right, cand) of
        (PInterface "Array" [t], PInterface "Array" [u]) -> do
            generateCreate right v
                ('{': f ++ ".len, alloc(" ++ f ++ ".len*sizeof("
                ++ ctype right "" ++ "))}")
            generateAssign (v++".len") (f++".len")
            ctr <- tmpVar
            generateCreate pInteger ctr "0"
            generateWhile (ctr++"<"++f++".len")
            var <- tmpVar
            checktype var (f++".arr["++ctr++"]") t u
            generateAssign (v++".arr["++ctr++"]") var
            generateAssign ctr (ctr ++ "+1")
            generateEnd
        _ -> do
            unless (cand == right) (typemismatch right cand)
            generateCreate right v f

createModelObject :: String -> String -> PDatatype -> [PDatatype]
                     -> PDatatype -> Compiler ()
createModelObject v f dt models from = do
    generateCreate dt v ("alloc(sizeof(struct "++pdt2str dt++"))")
    ms <- concat <$> mapM getModelMethods models
    forM_ ms $ \m -> do
        generateAssign (v++"->"++sname m) (pdt2str from++'_':pdt2str dt++'_':sname m)
        ensureMethodIsDefined from dt (sname m)
    when (length models > 1) $ forM_ (combinations models) $ \c -> do
        var <- tmpVar
        createModelObject var f (sumOrModel c) c from
        generateAssign (v++"->"++concatMap pdt2str c) var
    generateAssign (v++"->_obj") ("alloc(sizeof("++ctype from ""++"))")
    generateAssign ("*("++ctype from "*"++")"++v++"->_obj") f

sumOrModel :: [PDatatype] -> PDatatype
sumOrModel [t] = t
sumOrModel ts = PSum ts

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

ensureStructIsDefined :: PDatatype -> Compiler ()
ensureStructIsDefined dt =
    case dt of
        PInterface "Array" [a] -> do
            let n = "Array1" ++ pdt2str a
            conditionallyCreateStruct n $
                lift $ generateSuperHeaderCode
                    ("struct "++n++"{int len;" ++ ctype a "*arr" ++ ";};\n")
        dt@(PInterface t as@(p:ps)) -> do
            scope <- get
            let n = pdt2str dt
            when (isJust $ Map.lookup t $ models scope) $
                conditionallyCreateStruct n $ do
                    methods <- getModelMethods dt
                    compileStruct dt n methods
            let s = Map.lookup t $ structs scope
            case s of
                Just s' ->
                    conditionallyCreateStruct n $ do
                        ss <- getSubstitutions dt
                        queueDecl ss (Stc s')
                _ -> return ()
        dt@(PSum dts) -> do
            let n = pdt2str dt
            conditionallyCreateStruct n $ do
                methods <- concat <$> mapM getModelMethods dts
                compileStruct dt n methods
        _   -> return ()

ensureMethodIsDefined :: PDatatype -> PDatatype -> String -> Compiler ()
ensureMethodIsDefined dt model fname = do
    mf' <- getModelMethod model fname
    case mf' of
        Just mf -> conditionallyCreateMethod
            (fname ++ "_" ++ pdt2str model ++ "_" ++ sname mf) $
            compileStructMethod model mf dt
        Nothing -> tellError ("Unknown method `" ++ fname ++ "'")

ensureExtendIsDefined :: PDatatype -> Extend -> Compiler ()
ensureExtendIsDefined dt@(PInterface _ ts) extend = unless (null $ eTypeparameters extend) $ do
    let ss = Map.fromList $ zip (eTypeparameters extend) ts
    conditionallyCreateExtend (show dt ++ show (model extend)) $
        queueDecl ss $ Ext extend

-- Pääfunktio

compile :: [Declaration] -> Generator ()
compile decls = do
    let pvars = map decl2pvar
                    $ concatMap (\d -> case d of
                                    Func f -> [f]
                                    _     -> []) decls
    let models = Map.fromList $
                concatMap (\d -> case d of
                    Mdl m -> [(modelName m, m)]
                    _     -> []) decls
    let extends = concatMap (\d -> case d of
                    Ext e -> [(dtName e, e)]
                    _     -> []) decls
    let supers = collapse extends
    let structs = Map.fromList $
                concatMap (\d -> case d of
                    Stc s -> [(stcName s, s)]
                    _     -> []) decls
    generateHeaderCode "#include <stdlib.h>\n"
    generateHeaderCode "#include <gc.h>\n"
    generateHeaderCode "void * alloc(size_t x) { return GC_malloc(x); }\n"
    let scope = Scope {
      varscope = VarScope {
        functionName = "",
        variables = Map.empty,
        expectedReturnType = PNothing,
        subs = Map.empty
      },
      counter = 0,
      extends = supers,
      models = models,
      structs = structs,
      definedStructs = [],
      definedMethods = [],
      definedExtends = [],
      generatorQueue = [],
      declarationQueue = []
    }
    runStateT (do -- TODO siisti tämä
        rec (\ds -> do
            mapM_ (compileDecl pvars) ds
            s@Scope { declarationQueue = q } <- get
            put s { declarationQueue = [] }
            return [q]) $ map (\a -> (Map.empty, a)) decls
        scope' <- get
        forM_ (generatorQueue scope') id
     ) scope
    return ()

-- Kääntää yksittäisen funktion

compileDecl :: [PVariable] -> (Subs, Declaration)
               -> Compiler ()
compileDecl pvars (ss, Func decl@Function { name = fname, parameters = params,
                                            returnType=rtype, body = Extern })
    = lift $ generateExternFunctionHeader (dt2pdt rtype) [] fname
compileDecl pvars (ss, Func func) = do
    scope <- get
    let fname = name func
    params <- mapM (\(n,d) -> substitute ss d >>= \d' -> return (n,d')) (parameters func)
    rtype <- substitute ss $ returnType func
    let vscope = varscope scope
    put scope { varscope = vscope {
        functionName = fname,
        variables = Map.fromList $ pvars ++ params,
        expectedReturnType = rtype,
        subs = ss
    }}
    lift $ generateFunctionHeader rtype (map snd params) fname
    generateFunction rtype fname params
    compileStatement $ body func
    generateEnd
compileDecl _ (ss, Mdl m) =
    when (null $ typeparameters m) $ do
        let mname = modelName m
        let dt = PInterface mname [] --TODO
        fs <- mapM (subsFunction Map.empty) $ methods m
        compileStruct dt mname fs
compileDecl _ (ss, Ext Extend { dtName = n, model = m,
                                eMethods = fs, eTypeparameters = tps}) =
    when (null tps || not (null ss)) $ do
        dt <- substitute ss m
        forM_ fs (\f -> do
            mf' <- getModelMethod dt (name f)
            if isNothing mf'
                then tellError ("Invalid extension of " ++ n ++ " with "
                                ++ name f ++ "(): no such method in " ++ show dt)
                else do
                    let (Just mf) = mf'
                    let (Just etas) = forM tps (`Map.lookup` ss)
                    let edt = PInterface n etas
                    queueDecl ss $ Func f {
                        name = '_' : pdt2str edt ++ "_" ++ name f,
                        parameters = ("this", Typename n (map Typeparam tps)) : parameters f
                    }
         )
compileDecl _ (ss, Stc Struct { stcName = n, stcTypeparameters = tps, stcFields = fs }) =
    when (null tps || not (null ss)) $ do
        let (Just etas) = forM tps (`Map.lookup` ss)
        let dt = PInterface n etas
        lift $ generateHeaderCode ("struct " ++ pdt2str dt ++ "{\n")
        forM_ fs $ \(fname, ftype) -> do
            ftype' <- substitute ss ftype
            lift $ generateHeaderCode ("    " ++ ctype ftype' fname ++ ";\n")
        lift $ generateHeaderCode "};\n"

compileStruct :: PDatatype -> String -> [FSignature] -> Compiler ()
compileStruct dt mname methods = do
    lift $ generateHeaderCode ("struct " ++ mname ++ "{\n")
    forM_ methods $ \f ->
        let r = sreturnType f `ifDollar` dt
            ps = dt : map snd (sparameters f)
        in lift $ generateHeaderCode ("    " ++
            ctype r ('(':'*':sname f ++ ")(" ++ cparams ps ++ ")") ++ ";\n")
    case dt of
        PSum dts -> forM_ (combinations dts) $ \c ->
            lift $ generateHeaderCode ("    " ++ ctype (sumOrModel c) (concatMap pdt2str c)
                                       ++ ";\n")
        _ -> return ()
    lift $ generateHeaderCode "    void *_obj;\n};\n"

compileStructMethod :: PDatatype -> FSignature -> PDatatype -> Compiler ()
compileStructMethod m mf dt = do
    let mr = sreturnType mf `ifDollar` m
        r = sreturnType mf `ifDollar` dt
        ps = ctype m "this"
            : map (\(n,d) -> ctype d n) (sparameters mf)
        ps' = ("*(("++ctype dt "*" ++")this->_obj)")
            : map fst (sparameters mf)
        signature = ctype mr
            (pdt2str dt ++ "_" ++ pdt2str m ++ "_" ++ sname mf ++ "(" ++ joinColon ps ++ ")")
    lift $ generateHeaderCode (signature ++ ";\n")
    generateLater $ do
        lift $ generateCode (signature ++ " {\n")
        generateCreate r "_ret" ("_" ++ pdt2str dt ++ "_" ++ sname mf ++ "(" ++ joinColon ps' ++ ")")
        checktype "_ret2" "_ret" mr r
        generateReturn "_ret2"
        lift $ generateCode "\n}\n"

-- Lauseiden kääntäjät

compileStatement :: Statement -> Compiler ()
compileStatement (Block stmts)
    = do scope <- saveScope
         mapM_ compileStatement stmts
         restoreScope scope
compileStatement (Create name expr)
    = do cdt <- getVar name
         when (isJust cdt) $
            tellError ("variable " ++ name ++ " already exists")
         dt <- compileExpression name expr
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
            PInterface "Array" [dt']
                -> do scope <- saveScope
                      putVar name dt'
                      generateCreate dt' name (var++".arr["++ctr++"]")
                      compileStatement body
                      generateAssign ctr (ctr ++ "+1")
                      restoreScope scope
            _   -> typemismatch (pArray PNothing) dt
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
         generateCreate (pArray dt) v ('{': show (length exprs + 1) ++ ", alloc("
                                      ++ show (length exprs + 1)
                                      ++ "*sizeof(" ++ ctype dt "" ++ "))}")
         generateAssign (v++".len") (show $ length exprs + 1)
         generateAssign (v++".arr[0]") var
         mapM_ (\(i,val) -> do var' <- tmpVar
                               vt <- compileExpression var' val
                               var'' <- tmpVar
                               checktype var'' var' dt vt
                               generateAssign (v ++ ".arr["++show i++"]") var''
               ) (zip [1..length exprs] exprs)
         return (pArray dt)
compileExpression v (NewList dt size)
    = do ss <- getCurrentSubs
         pdt <- substitute ss dt
         var <- tmpVar
         vt <- compileExpression var size
         var' <- tmpVar
         checktype var' var pInteger vt
         generateCreate (pArray pdt) v ('{': var' ++ ", alloc("
                                      ++ var'
                                      ++ "*sizeof(" ++ ctype pdt "" ++ "))}")
         return (pArray pdt)
compileExpression v (NewStruct dt fieldValues) = do
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    fs <- getFields pdt
    case fs of
        Just fs' -> do
            fieldcodes <- checkargs (map snd fs') fieldValues
            generateCreate pdt v ("alloc(sizeof(struct "++pdt2str pdt ++ "))")
            forM_ (zip fs' fieldcodes) $ \((n, _), c) ->
                generateAssign (v++"->"++n) c
            return pdt
        Nothing -> do
            tellError ("struct not found: " ++ show dt)
            return PNothing
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
compileMethodCall v obj (PInterface "Str" []) "op_add" args
    = do checkargs [pString] args
         return pString

-- PInteger
compileMethodCall v obj (PInterface "Int" []) method args
    | method == "op_add" || method == "op_sub" || method == "op_mul" || method == "op_div"
        || method == "op_mod"
        = do (var:vars) <- checkargs [pInteger] args
             generateCreate pInteger v (obj++coperator method++var)
             return pInteger
    | method == "op_eq" || method == "op_neq" || method == "op_lt" || method == "op_gt"
        || method == "op_le" || method == "op_ge"
        = do (var:vars) <- checkargs [pInteger] args
             generateCreate pBool v (obj++coperator method++var)
             return pBool

-- PInteger
compileMethodCall v obj (PInterface "Int" []) method args
    | method == "op_and" || method == "op_or"
        = do (var:vars) <- checkargs [pBool] args
             generateCreate pBool v (obj++coperator method++var)
             return pBool

-- PArray
compileMethodCall v obj (PInterface "Array" [dt]) method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             generateCreate dt v (obj ++ ".arr["++index++"]")
             return dt
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             generateAssign (obj ++ ".arr["++index++"]") value
             generateCreate dt v value
             return dt
    | method == "length"
        = do checkargs [] args
             generateCreate pInteger v (obj ++ ".len")
             return pInteger
    | method == "ptr"
        = do checkargs [] args
             generateCreate (pPointer dt) v (obj ++ ".arr")
             return pInteger

-- PPointer
compileMethodCall v obj (PInterface "Pointer" [dt]) method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             generateCreate dt v (obj ++ "["++index++"]")
             return dt
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             generateAssign (obj ++ "["++index++"]") value
             generateCreate dt v value
             return dt

-- Määrittelemättömät metodit
compileMethodCall v obj dt@(PSum dts) method args
    = do ms <- mapM (\dt -> do f <- getModelMethod dt method;
                               return $ do {f' <- f; return (dt, f') } ) dts
         let m = firstJust ms
         case m of
            Nothing -> do
                tellError ("type " ++ show dt ++ " does not have method `" ++ method ++ "'")
                return PNothing
            Just (dt', m') ->
                compileMethodCall' v m' (obj++"->"++method) obj dt args
compileMethodCall v obj dt method args
    = do m <- getDTypeMethod dt method
         case m of
            Nothing -> do p <- getModelMethod dt method
                          case p of
                            Nothing -> do
                                tellError ("type " ++ show dt
                                    ++ " does not have method `" ++ method ++ "'")
                                return PNothing
                            Just p' ->
                                compileMethodCall' v p' (obj++"->"++method) obj dt args
            Just m' -> compileMethodCall' v m' ('_':pdt2str dt++'_':method) obj dt args

compileMethodCall' v f n obj dt args = do
    let r = sreturnType f `ifDollar` dt
    argcodes <- checkargs (map snd (sparameters f)) args
    generateCreate r v
        (n++"("++joinColon (obj:argcodes)++")")
    return r

coperator :: String -> String
coperator "op_or"  = "||"
coperator "op_and" = "&&"
coperator "op_eq"  = "=="
coperator "op_neq" = "!="
coperator "op_lt"  = "<"
coperator "op_gt"  = ">"
coperator "op_le"  = "<="
coperator "op_ge"  = ">="
coperator "op_add" = "+"
coperator "op_sub" = "-"
coperator "op_mul" = "*"
coperator "op_div" = "/"
coperator "op_mod" = "%"
