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
    = do let paramS = joinComma $ map (\(n,t) -> ctype t n) params
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
checktype v f right@(PInterface "Str" []) (PInterface "Pointer" [PInterface "Char" []]) =
    generateCreate right v f
checktype v f right cand = do
    es <- getExtends cand
    ms <- getSubstitutedExtends cand
    forM_ es $ ensureExtendIsDefined cand
    if right `elem` ms
        then createModelObject v f right [right] cand
        else case (right, cand) of
        (PInterface "Array" [t], PInterface "Array" [u]) ->
           if t /= u then do
            generateCreate right v
                ('{': f ++ ".len, alloc(" ++ f ++ ".len*sizeof("
                ++ ctype t "" ++ "))}")
            generateAssign (v++".len") (f++".len")
            ctr <- tmpVar
            generateCreate pInteger ctr "0"
            generateWhile (ctr++"<"++f++".len")
            var <- tmpVar
            checktype var (f++".ptr["++ctr++"]") t u
            generateAssign (v++".ptr["++ctr++"]") var
            generateAssign ctr (ctr ++ "+1")
            generateEnd
           else
            generateCreate right v f
        _ -> do
            unless (cand == right) (typemismatch right cand)
            generateCreate right v f

createModelObject :: String -> String -> PDatatype -> [PDatatype]
                     -> PDatatype -> Compiler ()
createModelObject v f dt models from = do
    generateCreate dt v ("alloc(sizeof(struct _"++pdt2str dt++"))")
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
                                  (\(t,v) -> compileExpressionAs t v)

ensureStructIsDefined :: PDatatype -> Compiler ()
ensureStructIsDefined dt =
    case dt of
        PInterface "Array" [a] -> do
            let n = "_Array1" ++ pdt2str a
            conditionallyCreateStruct n $ do
                lift $ generateSuperHeaderCode
                    ("typedef struct " ++n ++ " " ++ n ++ ";\n")
                lift $ generateSuperHeaderCode
                    ("struct "++n++"{int len;" ++ ctype a "*ptr" ++ ";};\n")
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
            (pdt2str dt ++ "_" ++ pdt2str model ++ "_" ++ sname mf) $
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
    let pvars = ("true",pBool):("false",pBool):map decl2pvar
                    (concatMap (\d -> case d of
                                    Func f -> [f]
                                    _     -> []) decls)
    let models = Map.fromList $
                concatMap (\d -> case d of
                    Mdl m -> [(modelName m, m)]
                    _     -> []) decls
    let extends = concatMap (\d -> case d of
                    Ext e -> [(dtName e, e)]
                    _     -> []) decls
    let supers = collapse extends

    let listStruct = Struct "Array" ["T"] [
            ("len", Typename "Int" []),
            ("ptr", Typename "Pointer" [Typeparam "T"])
            ] True

    let structs = Map.fromList $ ("Array", listStruct) :
                concatMap (\d -> case d of
                    Stc s -> [(stcName s, s)]
                    _     -> []) decls
    generateHeaderCode "#include <stdlib.h>\n"
    generateHeaderCode "#include <gc.h>\n"
    generateHeaderCode "#define true 1\n"
    generateHeaderCode "#define false 0\n"
    generateHeaderCode "void * alloc(size_t x) { return GC_malloc(x); }\n"
    let scope = Scope {
      varscope = VarScope {
        functionName = "",
        variables = Map.empty,
        expectedReturnType = PNothing,
        subs = Map.empty,
        doesReturn = False
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
        subs = ss,
        doesReturn = False
    }}
    lift $ generateFunctionHeader rtype (map snd params) fname
    generateFunction rtype fname params
    compileStatement $ body func
    returns <- doesThisPathReturn
    when (rtype /= pVoid && not returns) $
        tellError "function does not return a value"
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
        let (Just etas) = forM tps (`Map.lookup` ss)
        let edt = PInterface n etas
        prs <- getPrerequisites dt
        es <- getSubstitutedExtends edt
        forM_ prs $ \prt ->
            unless (prt `elem` es) $
                tellError ("extension of " ++ show edt ++ " with " ++ show dt ++
                           " does not satisfy the interface: it does not implement " ++
                           show prt ++ ", which is a prerequisite")
        ms <- getModelMethods dt
        unless (length ms == length fs) $
            tellError ("extension of " ++ show edt ++ " with " ++ show dt ++
                       "does not satisfy the interface: there should be " ++
                       show (length ms) ++ " methods, not " ++ show (length fs))
        forM_ fs (\f -> do
            mf' <- getModelMethod dt (name f)
            case mf' of
                Nothing -> tellError ("invalid extension of " ++ show edt ++ " with method "
                                      ++ name f ++ ": no such method in " ++ show dt)
                Just mf -> do
                    forM_ (zip (returnType f:map snd (parameters f))
                        (sreturnType mf:map snd (sparameters mf))) $
                        \(dt', mpdt') -> do
                            pdt <- substitute ss dt'
                            let mpdt = mpdt' `ifDollar` edt
                            unless (pdt == mpdt) $
                                tellError ("extension method " ++ show edt ++ "." ++ name f ++
                                           " does not satisfy the interface defined in " ++
                                           show dt ++ ": expected " ++ show mpdt ++
                                           ", got " ++ show pdt)
                    queueDecl ss $ Func f {
                        name = '_' : pdt2str edt ++ "_" ++ name f,
                        parameters = ("this", Typename n (map Typeparam tps)) : parameters f
                    }
         )
compileDecl _ (ss, Stc Struct { stcName = n, stcTypeparameters = tps, stcFields = fs,
                                isConst = c }) =
    when (null tps || not (null ss)) $ do
        let (Just etas) = forM tps (`Map.lookup` ss)
        let dt = PInterface n etas
        lift $ generateSuperHeaderCode ("typedef struct _" ++ pdt2str dt ++
                                        (if c then " " else "* ") ++ pdt2str dt ++ ";\n")
        lift $ generateSuperHeaderCode ("struct _" ++ pdt2str dt ++ "{\n")
        forM_ fs $ \(fname, ftype) -> do
            ftype' <- substitute ss ftype
            lift $ generateSuperHeaderCode ("    " ++ ctype ftype' fname ++ ";\n")
        lift $ generateSuperHeaderCode "};\n"

compileStruct :: PDatatype -> String -> [FSignature] -> Compiler ()
compileStruct dt mname methods = do
    lift $ generateSuperHeaderCode ("typedef struct _" ++ mname ++ "* " ++
                                    mname ++ ";\n")
    lift $ generateSuperHeaderCode ("struct _" ++ mname ++ "{\n")
    forM_ methods $ \f ->
        let r = sreturnType f `ifDollar` dt
            ps = dt : map snd (sparameters f)
        in lift $ generateSuperHeaderCode ("    " ++
            ctype r ('(':'*':sname f ++ ")(" ++ cparams ps ++ ")") ++ ";\n")
    case dt of
        PSum dts -> forM_ (combinations dts) $ \c ->
            lift $ generateSuperHeaderCode ("    " ++ ctype (sumOrModel c) (concatMap pdt2str c)
                                       ++ ";\n")
        _ -> return ()
    lift $ generateSuperHeaderCode "    void *_obj;\n};\n"

compileStructMethod :: PDatatype -> FSignature -> PDatatype -> Compiler ()
compileStructMethod m mf dt = do
    let mr = sreturnType mf `ifDollar` m
        r = sreturnType mf `ifDollar` dt
        ps = ctype m "this"
            : map (\(n,d) -> ctype d n) (sparameters mf)
        ps' = ("*(("++ctype dt "*" ++")this->_obj)")
            : map fst (sparameters mf)
        signature = ctype mr
            (pdt2str dt ++ "_" ++ pdt2str m ++ "_" ++ sname mf ++ "(" ++ joinComma ps ++ ")")
    lift $ generateHeaderCode (signature ++ ";\n")
    generateLater $ do
        lift $ generateCode (signature ++ " {\n")
        generateCreate r "_ret" ("_" ++ pdt2str dt ++ "_" ++ sname mf ++
                                 "(" ++ joinComma ps' ++ ")")
        checktype "_ret2" "_ret" mr r
        generateReturn "_ret2"
        lift $ generateCode "\n}\n"

-- Lauseiden kääntäjät

compileStatement :: Statement -> Compiler ()
compileStatement (Block stmts)
    = do scope <- saveScope
         forM_ stmts $ \stmt -> do
            doesThisPathReturn >>= \a -> when a $ tellError "unreachable code"
            compileStatement stmt
         returns <- doesThisPathReturn -- tarkistetaan, poistutaanko lohkon sisällä funktiosta
         restoreScope scope
         when returns thisPathReturns -- tallennetaan tieto myös palautettuun scopeen
compileStatement (Create name expr)
    = do cdt <- getVar name
         when (isJust cdt) $
            tellError ("variable " ++ name ++ " already exists")
         dt <- compileExpression name PNothing expr
         putVar name dt
compileStatement (Assign name expr)
    = do dt <- getVarOrError name
         var <- tmpVar
         vt <- compileExpression var dt expr
         var' <- tmpVar
         checktype var' var dt vt
         generateAssign name var'
compileStatement (If expr thenBody elseBody)
    = do var <- compileExpressionAs pBool expr
         generateIf var
         -- tallennetaan scope ja käännetään then-osa. jos siellä on return-lause,
         -- pistetään muistiin
         scope <- saveScope
         compileStatement thenBody
         thenReturns <- doesThisPathReturn
         restoreScope scope
         -- käännetään else-osa ja pistetään samalla tavalla muistiin,
         -- jos siellä on return-lause
         elseReturns <- case elseBody of
            Just stmt -> do generateElse
                            scope <- saveScope
                            compileStatement stmt
                            er <- doesThisPathReturn
                            restoreScope scope
                            return er
            Nothing -> return False -- ei elseä, ei returnia
         -- jos molemmissa osissa on return, tämä if poistuu funktiosta varmasti
         when (thenReturns && elseReturns) thisPathReturns
         generateEnd
compileStatement (While expr body)
    = do var <- compileExpressionAs pBool expr
         generateWhile var
         scope <- saveScope
         compileStatement body
         restoreScope scope
         generateEnd
compileStatement (For name expr body)
    = do var <- tmpVar
         dt <- compileExpression var (pArray PNothing) expr
         ctr <- tmpVar
         generateCreate pInteger ctr "0"
         generateWhile (ctr++"<"++var++".len")
         case dt of
            PInterface "Array" [dt']
                -> do scope <- saveScope
                      putVar name dt'
                      generateCreate dt' name (var++".ptr["++ctr++"]")
                      compileStatement body
                      generateAssign ctr (ctr ++ "+1")
                      restoreScope scope
            _   -> typemismatch (pArray PNothing) dt
         generateEnd
compileStatement (Expr (Call expr args))
    = do var <- tmpVar
         dt <- compileExpression var (pFunction pVoid []) expr
         case dt of
            PInterface "Func" (retType:paramTypes)
                -> do argcodes <- checkargs paramTypes args
                      lift $ generateCode (var++"("++joinComma argcodes++");\n")
                      return ()
            _ -> do typemismatch (pFunction PNothing []) dt
                    return ()
compileStatement (Expr expr)
    = do var <- tmpVar
         compileExpression var pVoid expr
         return ()
compileStatement (Return expr)
    = do rt <- getExpectedReturnType
         var <- compileExpressionAs rt expr
         thisPathReturns
         generateReturn var

-- Lausekkeiden validaattorit

compileExpressionAs :: PDatatype -> Expression -> Compiler String
compileExpressionAs dt exp = do
    var' <- tmpVar
    dt' <- compileExpression var' dt exp
    var <- tmpVar
    checktype var var' dt dt'
    return var

compileExpression :: String -> PDatatype -> Expression -> Compiler PDatatype
compileExpression v expdt (Int i) = do
    generateCreate pInteger v (show i)
    return pInteger
compileExpression v expdt (Str s) = do
    generateCreate pString v (show s)
    return pString
compileExpression v expdt (Var name)
    = do dt <- getVarOrError name
         generateCreate dt v name
         return dt
compileExpression v expdt (Call expr args)
    = do var <- tmpVar
         dt <- compileExpression var (pFunction PNothing []) expr
         case dt of
            PInterface "Func" (retType:paramTypes)
                -> do argcodes <- checkargs paramTypes args
                      generateCreate retType v (var++"("++joinComma argcodes++")")
                      return retType
            _ -> do typemismatch (pFunction PNothing []) dt
                    return PNothing
compileExpression v expdt (List (expr:exprs))
    = do var <- tmpVar
         dt <- compileExpression var PNothing expr -- Käännetään ensimmäisen alkion koodi
                                                   -- tyypin selvittämiseksi
         generateCreate (pArray dt) v ('{': show (length exprs + 1) ++ ", alloc("
                                      ++ show (length exprs + 1)
                                      ++ "*sizeof(" ++ ctype dt "" ++ "))}")
         generateAssign (v++".len") (show $ length exprs + 1)
         generateAssign (v++".ptr[0]") var
         mapM_ (\(i,val) -> do var' <- compileExpressionAs dt val
                               generateAssign (v ++ ".ptr["++show i++"]") var'
               ) (zip [1..length exprs] exprs)
         return (pArray dt)
compileExpression v expdt (Range from to)
    = do fromv <- compileExpressionAs pInteger from
         tov <- compileExpressionAs pInteger to
         sizev <- tmpVar
         generateCreate pInteger sizev (tov ++ "-" ++ fromv ++ "+1")
         generateCreate (pArray pInteger) v
            ('{': sizev ++ ", alloc("
            ++ sizev
            ++ "*sizeof(" ++ ctype pInteger "" ++ "))}")
         countv <- tmpVar
         generateCreate pInteger countv "0"
         generateWhile (countv ++ "<" ++ sizev)
         generateAssign (v ++ ".ptr[" ++ countv ++ "]") (fromv ++ "+" ++ countv)
         generateAssign countv (countv ++ "+1")
         generateEnd
         return (pArray pInteger)
compileExpression v expdt (NewList dt size)
    = do ss <- getCurrentSubs
         pdt <- substitute ss dt
         var <- compileExpressionAs pInteger size
         generateCreate (pArray pdt) v ('{': var ++ ", alloc("
                                      ++ var
                                      ++ "*sizeof(" ++ ctype pdt "" ++ "))}")
         return (pArray pdt)
compileExpression v expdt (NewStruct dt fieldValues) = do
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    fs <- getFields pdt
    case fs of
        Just fs' -> do
            fieldvaluecodes <- checkargs (map snd fs') fieldValues
            (Just cons) <- isConstant pdt
            if not cons
             then do
                generateCreate pdt v ("alloc(sizeof(struct _"++pdt2str pdt ++ "))")
                forM_ (zip fs' fieldvaluecodes) $ \((n, _), c) ->
                    generateAssign (v++"->"++n) c
             else
                generateCreate pdt v ("{" ++ joinComma fieldvaluecodes ++ "}")
            return pdt
        Nothing -> do
            tellError ("struct not found: " ++ show dt)
            return PNothing
compileExpression v expdt (NewPtrList dt size)
    = do ss <- getCurrentSubs
         pdt <- substitute ss dt
         var <- compileExpressionAs pInteger size
         generateCreate (pPointer pdt) v ("alloc("
                                      ++ var
                                      ++ "*sizeof(" ++ ctype pdt "" ++ "))")
         return (pPointer pdt)
compileExpression v expdt (FieldGet obj field) = do
    var <- tmpVar
    dt <- compileExpression var PNothing obj
    ifFieldExists dt field $ \t -> do
        fieldcode <- structField dt var field
        generateCreate t v fieldcode
        return t
compileExpression v expdt (FieldSet obj field val) = do
    var <- tmpVar
    dt <- compileExpression var PNothing obj
    ifFieldExists dt field $ \t -> do
        var2 <- tmpVar
        dt2 <- compileExpression var2 dt val
        var3 <- tmpVar
        checktype var3 var2 t dt2
        fieldcode <- structField dt var field
        generateAssign fieldcode var3
        generateCreate dt2 v var2
        return dt2
compileExpression v expdt (MethodCall obj method args)
    = do var <- tmpVar
         dt <- compileExpression var PNothing obj
         compileMethodCall v var dt method args

-- suorittaa annetun koodin, jos kenttä on olemassa
ifFieldExists :: PDatatype -> String -> (PDatatype -> Compiler PDatatype)
                 -> Compiler PDatatype
ifFieldExists dt field callback = do
    fs <- getFields dt
    case fs of
        Just fs' -> do
            let t = lookup field fs'
            case t of
                Just t' ->
                    callback t'
                Nothing -> do
                    tellError ("struct type " ++ show dt ++
                               " does not have field `" ++ field ++ "'")
                    return PNothing
        Nothing -> do
            tellError ("non-struct type " ++ show dt ++
                       " does not have field `" ++ field ++ "'")
            return PNothing

-- palauttaa C-lausekkeen rakenteen kenttää varten
structField :: PDatatype -> String -> String -> Compiler String
structField dt var field = do
    (Just constant) <- isConstant dt
    let accessOp = if constant then "." else "->" -- Jos rakenne on vakio,
    return (var++accessOp++field)                 -- tyyppi ei ole pointteri


-- Sisäänrakennettujen tyyppien metodit

type MethodCallCompiler = String -> String
                     -> PDatatype -> String -> [Expression]
                     -> Compiler PDatatype

compileMethodCall :: MethodCallCompiler

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
             generateCreate dt v (obj ++ ".ptr["++index++"]")
             return dt
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             generateAssign (obj ++ ".ptr["++index++"]") value
             generateCreate dt v value
             return dt

-- PPointer
compileMethodCall v obj (PInterface "Pointer" [dt]) method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             generateCreate dt v (obj ++ "["++index++"]")
             tellWarning "pointer arithmetic is not typesafe"
             return dt
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             generateAssign (obj ++ "["++index++"]") value
             generateCreate dt v value
             tellWarning "pointer arithmetic is not typesafe"
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
            Nothing -> do
                p <- getModelMethod dt method
                case p of
                  Nothing -> do
                      tellError ("type " ++ show dt ++
                                 " does not have method `" ++ method ++ "'")
                      return PNothing
                  Just p' ->
                      compileMethodCall' v p' (obj++"->"++method) obj dt args
            Just (e, m') -> do
                ensureExtendIsDefined dt e
                compileMethodCall' v m' ('_':pdt2str dt++'_':method) obj dt args

compileMethodCall' v f n obj dt args = do
    let r = sreturnType f `ifDollar` dt
    argcodes <- checkargs (map snd (sparameters f)) args
    generateCreate r v
        (n++"("++joinComma (obj:argcodes)++")")
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

