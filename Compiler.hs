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

generateIf :: String -> Compiler () -> Compiler ()
generateIf cond callback = do
    lift $ tell ["    if(", cond, "){\n"]
    callback
    generateEnd

generateWhile :: String -> Compiler () -> Compiler ()
generateWhile cond callback = do
    lift $ tell ["    while(", cond, "){\n"]
    callback
    generateEnd

generateElse :: Compiler  ()
generateElse
    = lift $ tell ["    }else{\n"]

generateEnd :: Compiler ()
generateEnd
    = lift $ tell ["    }\n"]

generateCreate :: PDatatype -> String -> String -> Compiler ()
generateCreate dt var value
    = do lift $ tell ["    ", ctype dt var,"=",value,";\n"]
         ensureStructIsDefined dt -- varmistetaan, että kaikki tyypin käsittelemiseen
                                  -- tarvittavat c-tietorakenteet ja funktiot on generoitu

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

-- toteuttaa tarvittavat tyyppimuunnokset
-- luo uuden muuttujan ensimmäisen parametrin nimisenä
checktype :: String -> String -> PDatatype -> PDatatype -> Compiler ()

-- intersektiotyypin muuntaminen sen osaksi (esim. A&B -> A) ei vielä onnistu
checktype v f right cand@(PSum dts)
    = do unless (cand == right) (typemismatch right cand)
         generateCreate right v f
-- tyypin muuntaminen intersektiotyypiksi
checktype v f right@(PSum dts) cand
    = createModelObject v f right dts cand
-- muunnos Char* -> Str
checktype v f right@(PInterface "Str" []) (PInterface "Pointer" [PInterface "Char" []]) =
    generateCreate right v f
-- muunnos Str -> Char*
checktype v f right@(PInterface "Pointer" [PInterface "Char" []]) (PInterface "Str" []) =
    generateCreate right v f
-- muut muunnokset
checktype v f right cand = do
    -- varmistetaan, että tyypin laajennokset on generoitu
    es <- getExtends cand
    forM_ es $ ensureExtendIsDefined cand
    -- jos tyypit ovat samat, tyyppimuunnosta ei tarvitse tehdä
    if right == cand then
        generateCreate right v f
     else do
        -- katsotaan yritetäänkö tyyppiä muuttaa mallityypiksi
        ms <- getSubstitutedExtends cand
        if right `elem` ms
         then createModelObject v f right [right] cand
         -- jos ei, katsotaan voidaanko array muuttaa kovarianttiutensa takia
         else case (right, cand) of
            (PInterface "Array" [t], PInterface "Array" [u]) -> do
                generateCreate right v
                    ('{': f ++ ".len, alloc(" ++ f ++ ".len*sizeof("
                    ++ ctype t "" ++ "))}")
                generateAssign (v++".len") (f++".len")
                ctr <- tmpVar
                generateCreate pInteger ctr "0"
                generateWhile (ctr++"<"++f++".len") $ do
                    var <- tmpVar
                    checktype var (f++".ptr["++ctr++"]") t u
                    generateAssign (v++".ptr["++ctr++"]") var
                    generateAssign ctr (ctr ++ "+1")
            -- muissa tapauksissa tyyppimuunnosta ei voida tehdä
            _ -> typemismatch right cand

-- luodaan malli- tai intersektiotyypin olio
createModelObject :: String -> String -> PDatatype -> [PDatatype]
                     -> PDatatype -> Compiler ()
createModelObject v f dt models from = do
    generateCreate dt v ("alloc(sizeof(struct _"++pdt2str dt++"))")
    ms <- concat <$> mapM getModelMethods models
    -- tallennetaan olion vtableen metodipointterit
    forM_ ms $ \m -> do
        generateAssign (v++"->"++sname m) (pdt2str from++'_':pdt2str dt++'_':sname m)
        ensureMethodIsDefined from dt (sname m)
    -- luodaan alatyyppien oliot (tätä voisi vähän optimoida)
    when (length models > 1) $ forM_ (combinations models) $ \c -> do
        var <- tmpVar
        createModelObject var f (sumOrModel c) c from
        generateAssign (v++"->"++concatMap pdt2str c) var
    generateAssign (v++"->_obj") ("alloc(sizeof("++ctype from ""++"))")
    generateAssign ("*("++ctype from "*"++")"++v++"->_obj") f

sumOrModel :: [PDatatype] -> PDatatype
sumOrModel [t] = t
sumOrModel ts = PSum ts

-- kääntää listan lausekkeita ja muuntaa arvot annetuiksi tyypeiksi
checkargs :: [PDatatype] -> [Expression] -> Compiler [String]
checkargs paramTs args = if length paramTs /= length args
                            then do tellError ("wrong number of arguments: "
                                              ++ show (length paramTs) ++ " required, got "
                                              ++ show (length args))
                                    return []
                            else forM (zip paramTs args)
                                  (\(t,v) -> compileExpressionAs t v)

-- varmistaa, että tyypin vaatimat C-structit ja funktiot on luotu
ensureStructIsDefined :: PDatatype -> Compiler ()
ensureStructIsDefined dt =
    case dt of
        -- array tarvitsee oman structinsa
        PInterface "Array" [a] -> do
            let n = "_Array1" ++ pdt2str a
            conditionallyCreateStruct n $ do
                lift $ generateSuperHeaderCode
                    ("typedef struct " ++n ++ " " ++ n ++ ";\n")
                lift $ generateSuperHeaderCode
                    ("struct "++n++"{int len;" ++ ctype a "*ptr" ++ ";};\n")
        dt@(PInterface t as@(p:ps)) -> do
            scope <- get
            -- luodaan tyypin laajennokset, jos niitä ei ole jo luotu
            -- tämä tehdään vähän purkasti tässä funktiossa...
            es <- getExtends dt
            forM_ es $ ensureExtendIsDefined dt
            -- luotavan structin nimi
            let n = pdt2str dt
            -- luodaan malliolion struct, jos tyyppi on malli
            when (isJust $ Map.lookup t $ models scope) $
                conditionallyCreateStruct n $ do
                    methods <- getModelMethods dt
                    compileStruct dt n methods
            -- jos tyyppi on struct, luodaan sille oma C-struct
            let s = Map.lookup t $ structs scope
            case s of
                Just s' ->
                    conditionallyCreateStruct n $ do
                        ss <- getSubstitutions dt
                        queueDecl ss (Stc s')
                _ -> return ()
        -- intersektiotyypin struct on samanlainen kuin malliolion, mutta sen
        -- vtable sisältää kaikkien mallien metodit
        dt@(PSum dts) -> do
            let n = pdt2str dt
            conditionallyCreateStruct n $ do
                methods <- concat <$> mapM getModelMethods dts
                compileStruct dt n methods
        _   -> return ()

-- varmistaa, että tietyn malli- tai intersektiotyypin metodi on olemassa
-- tämä on siis sellainen funktio, joka muuntaa mallin tai intersektiotyypin
-- olion varsinaiseksi arvoksi, joka sitten annetaan varsinaiselle metodifunktiolle
-- esim. dt=Int, model=String, fname=toString, muuntaa Stringin Intiksi ja antaa sen
-- metodille Int_toString
ensureMethodIsDefined :: PDatatype -> PDatatype -> String -> Compiler ()
ensureMethodIsDefined dt model fname = do
    mf' <- getModelMethod model fname
    case mf' of
        Just mf -> conditionallyCreateMethod
            (pdt2str dt ++ "_" ++ pdt2str model ++ "_" ++ sname mf) $
            compileStructMethod model mf dt
        Nothing -> tellError ("Unknown method `" ++ fname ++ "'")

-- varmistaa, että tyyppiparametrisoidun laajennoksen haluttu versio generoidaan
ensureExtendIsDefined :: PDatatype -> Extend -> Compiler ()
ensureExtendIsDefined dt@(PInterface _ ts) extend = unless (null $ eTypeparameters extend) $ do
    let ss = Map.fromList $ zip (eTypeparameters extend) ts
    conditionallyCreateExtend (show dt ++ show (model extend)) $
        queueDecl ss $ Ext extend

-- Pääfunktio

compile :: [Declaration] -> Generator ()
compile decls = do
    let pvars = [("true",pBool), ("false",pBool)]
    let functions = Map.fromList $
                concatMap (\d -> case d of
                    Func f -> [(name f, f)]
                    _      -> []) decls
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
      functions = functions,
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

-- generoi malli- tai intersektiotyypin structin
compileStruct :: PDatatype -> String -> [FSignature] -> Compiler ()
compileStruct dt mname methods = do
    lift $ generateSuperHeaderCode ("typedef struct _" ++ mname ++ "* " ++
                                    mname ++ ";\n")
    lift $ generateSuperHeaderCode ("struct _" ++ mname ++ "{\n")
    -- generoidaan vtable
    forM_ methods $ \f ->
        let r = sreturnType f `ifDollar` dt
            ps = dt : map snd (sparameters f)
        in lift $ generateSuperHeaderCode ("    " ++
            ctype r ('(':'*':sname f ++ ")(" ++ cparams ps ++ ")") ++ ";\n")
    -- generoidaan kombinaatioiden oliot
    case dt of
        PSum dts -> forM_ (combinations dts) $ \c ->
            lift $ generateSuperHeaderCode ("    " ++ ctype (sumOrModel c) (concatMap pdt2str c)
                                       ++ ";\n")
        _ -> return ()
    lift $ generateSuperHeaderCode "    void *_obj;\n};\n"

-- generoi funktion, joka ottaa malli- tai intersektiotyypin sekä argumentteja
-- ja muuntaa mallin tai intersektion oikeaan muotoon dereferoimalla _obj-kentän
-- ja syöttää kaikki arvot varsinaiselle metodifunktiolle
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
         (var, dt) <- compileExpression PNothing expr
         generateCreate dt name var
         putVar name dt
compileStatement (Assign name expr)
    = do dt <- getVarOrError name
         var <- compileExpressionAs dt expr
         generateAssign name var
compileStatement (If expr thenBody elseBody)
    = do var <- compileExpressionAs pBool expr
         generateIf var $ do
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
compileStatement (While expr body) =
    generateWhile "1" $ do
        var <- compileExpressionAs pBool expr
        generateIf ("!("++var++")") $
            lift $ generateCode "break;\n"
        scope <- saveScope
        compileStatement body
        restoreScope scope
compileStatement (For name expr body)
    = do (var, dt) <- compileExpression (pArray PNothing) expr
         ctr <- tmpVar
         generateCreate pInteger ctr "0"
         generateWhile (ctr++"<"++var++".len") $
             case dt of
                PInterface "Array" [dt']
                    -> do scope <- saveScope
                          putVar name dt'
                          generateCreate dt' name (var++".ptr["++ctr++"]")
                          compileStatement body
                          generateAssign ctr (ctr ++ "+1")
                          restoreScope scope
                _   -> typemismatch (pArray PNothing) dt
compileStatement (Expr (Call name args)) = do
    (_, code) <- compileCall name args
    lift $ generateCode (code ++ ";\n")
compileStatement (Expr expr)
    = do (var, dt) <- compileExpression pVoid expr
         var' <- tmpVar
         generateCreate dt var' var -- purkkaa
         return ()
compileStatement (Return expr)
    = do rt <- getExpectedReturnType
         var <- compileExpressionAs rt expr
         thisPathReturns
         generateReturn var

compileCall name args = do
    f <- getFunction name
    case f of
        Just f' -> do
            let rt = dt2pdt $ returnType f'
            let ps = map (dt2pdt.snd) $ parameters f'
            argcodes <- checkargs ps args
            return (rt, name++"("++joinComma argcodes++")")
        Nothing -> do
            tellError ("function not found: `" ++ name ++ "'")
            return (PNothing, "NOTHING")

-- Lausekkeiden kääntäjät

-- kääntää lausekkeen ja muuttaa sen halutun tyyppiseksi
compileExpressionAs :: PDatatype -> Expression -> Compiler String
compileExpressionAs dt exp = do
    (exprv, dt') <- compileExpression dt exp
    if dt == dt'
     then return exprv
     else do
        var' <- tmpVar
        generateCreate dt' var' exprv
        var <- tmpVar
        checktype var var' dt dt'
        return var

compileExpression :: PDatatype -> Expression -> Compiler (String, PDatatype)
compileExpression expdt (Int i) =
    return (show i, pInteger)
compileExpression expdt (Str s) =
    return (show s, pString)
compileExpression expdt (Var name)
    = do dt <- getVarOrError name
         return (name, dt)
compileExpression expdt (Call name args) = do
    (retType, code) <- compileCall name args
    return (code, retType)
compileExpression expdt (List (expr:exprs)) = do
    var <- tmpVar
    (firstv, dt) <- compileExpression PNothing expr -- Käännetään ensimmäisen alkion koodi
                                                    -- tyypin selvittämiseksi
    generateCreate (pArray dt) var ('{': show (length exprs + 1) ++ ", alloc("
                              ++ show (length exprs + 1)
                              ++ "*sizeof(" ++ ctype dt "" ++ "))}")
    generateAssign (var++".len") (show $ length exprs + 1)
    generateAssign (var++".ptr[0]") firstv
    mapM_ (\(i,val) -> do valuecode <- compileExpressionAs dt val
                          generateAssign (var ++ ".ptr["++show i++"]") valuecode
       ) (zip [1..length exprs] exprs)
    return (var, pArray dt)
compileExpression expdt (Range from to) = do
    var <- tmpVar
    fromv <- compileExpressionAs pInteger from
    tov <- compileExpressionAs pInteger to
    sizev <- tmpVar
    generateCreate pInteger sizev (tov ++ "-" ++ fromv ++ "+1")
    generateCreate (pArray pInteger) var
        ('{': sizev ++ ", alloc("
        ++ sizev
        ++ "*sizeof(" ++ ctype pInteger "" ++ "))}")
    countv <- tmpVar
    generateCreate pInteger countv "0"
    generateWhile (countv ++ "<" ++ sizev) $ do
        generateAssign (var ++ ".ptr[" ++ countv ++ "]") (fromv ++ "+" ++ countv)
        generateAssign countv (countv ++ "+1")
    return (var, pArray pInteger)
compileExpression expdt (NewList dt size) = do
    var <- tmpVar
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    sizev' <- compileExpressionAs pInteger size
    sizev <- tmpVar
    generateCreate pInteger sizev sizev'
    generateCreate (pArray pdt) var ('{': sizev ++ ", alloc("
                                     ++ sizev
                                     ++ "*sizeof(" ++ ctype pdt "" ++ "))}")
    return (var, pArray pdt)
compileExpression expdt (NewStruct dt fieldValues) = do
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    fs <- getFields pdt
    case fs of
        Just fs' -> do
            var <- tmpVar
            fieldvaluecodes <- checkargs (map snd fs') fieldValues
            (Just cons) <- isConstant pdt
            if not cons
             then do
                generateCreate pdt var ("alloc(sizeof(struct _"++pdt2str pdt ++ "))")
                forM_ (zip fs' fieldvaluecodes) $ \((n, _), c) ->
                    generateAssign (var++"->"++n) c
             else
                generateCreate pdt var ("{" ++ joinComma fieldvaluecodes ++ "}")
            return (var, pdt)
        Nothing -> do
            tellError ("struct not found: " ++ show dt)
            return ("NOTHING", PNothing)
compileExpression expdt (NewPtrList dt size) = do
    var <- tmpVar
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    sizev <- compileExpressionAs pInteger size
    generateCreate (pPointer pdt) var ("alloc("
                                       ++ sizev
                                       ++ "*sizeof(" ++ ctype pdt "" ++ "))")
    return (var, pPointer pdt)
compileExpression expdt (FieldGet obj field) = do
    (var, dt) <- compileExpression PNothing obj
    ifFieldExists dt field ("NOTHING", PNothing) $ \t -> do
        fieldcode <- structField dt var field
        return (fieldcode, t)
compileExpression expdt (FieldSet obj field val) = do
    (var, dt) <- compileExpression PNothing obj
    ifFieldExists dt field ("NOTHING", PNothing) $ \t -> do
        valv <- tmpVar
        (valcode, dt2) <- compileExpression dt val
        generateCreate dt2 valv valcode
        var3 <- tmpVar
        checktype var3 valv t dt2
        fieldcode <- structField dt var field
        generateAssign fieldcode var3
        return (valv, dt2)
compileExpression expdt (Cast dt expr) = do
    ss <- getCurrentSubs
    pdt <- substitute ss dt
    var <- compileExpressionAs pdt expr
    return (var, pdt)
compileExpression expdt (MethodCall obj method args)
    = do (objv, dt) <- compileExpression PNothing obj
         compileMethodCall objv dt method args

-- suorittaa annetun koodin, jos kenttä on olemassa
ifFieldExists :: PDatatype -> String -> a -> (PDatatype -> Compiler a)
                 -> Compiler a
ifFieldExists dt field failv callback = do
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
                    return failv
        Nothing -> do
            tellError ("non-struct type " ++ show dt ++
                       " does not have field `" ++ field ++ "'")
            return failv

-- palauttaa C-lausekkeen rakenteen kenttää varten
structField :: PDatatype -> String -> String -> Compiler String
structField dt var field = do
    (Just constant) <- isConstant dt
    let accessOp = if constant then "." else "->" -- Jos rakenne on vakio,
    return (var++accessOp++field)                 -- tyyppi ei ole pointteri


-- Sisäänrakennettujen tyyppien metodit

type MethodCallCompiler = String
                     -> PDatatype -> String -> [Expression]
                     -> Compiler (String, PDatatype)

compileMethodCall :: MethodCallCompiler

-- PInteger
compileMethodCall obj (PInterface "Int" []) method args
    | method == "op_add" || method == "op_sub" || method == "op_mul" || method == "op_div"
        || method == "op_mod"
        = do (var:vars) <- checkargs [pInteger] args
             return ("("++obj++coperator method++var++")", pInteger)
    | method == "op_eq" || method == "op_neq" || method == "op_lt" || method == "op_gt"
        || method == "op_le" || method == "op_ge"
        = do (var:vars) <- checkargs [pInteger] args
             return ("("++obj++coperator method++var++")", pBool)

-- PInteger
compileMethodCall obj (PInterface "Int" []) method args
    | method == "op_and" || method == "op_or"
        = do (var:vars) <- checkargs [pBool] args
             return ("("++obj++coperator method++var++")", pBool)

-- PArray
compileMethodCall obj (PInterface "Array" [dt]) method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             return (obj ++ ".ptr["++index++"]", dt)
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             v <- tmpVar
             generateCreate dt v value
             generateAssign (obj ++ ".ptr["++index++"]") v
             return (v, dt)

-- PPointer
compileMethodCall obj (PInterface "Pointer" [dt]) method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             tellWarning "pointer arithmetic is not typesafe"
             return (obj ++ "["++index++"]", dt)
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             v <- tmpVar
             generateCreate dt v value
             generateAssign (obj ++ "["++index++"]") v
             tellWarning "pointer arithmetic is not typesafe"
             return (v, dt)

-- Määrittelemättömät metodit
compileMethodCall obj dt@(PSum dts) method args
    = do ms <- mapM (\dt -> do f <- getModelMethod dt method;
                               return $ do {f' <- f; return (dt, f') } ) dts
         let m = firstJust ms
         case m of
            Nothing -> do
                tellError ("type " ++ show dt ++ " does not have method `" ++ method ++ "'")
                return ("NOTHING", PNothing)
            Just (dt', m') -> do
                objv <- tmpVar
                generateCreate dt objv obj
                compileMethodCall' m' (objv++"->"++method) objv dt args
compileMethodCall obj dt method args
    = do m <- getDTypeMethod dt method
         case m of
            Nothing -> do
                p <- getModelMethod dt method
                case p of
                  Nothing -> do
                    tellError ("type " ++ show dt ++
                               " does not have method `" ++ method ++ "'")
                    return ("NOTHING", PNothing)
                  Just p' -> do
                    objv <- tmpVar
                    generateCreate dt objv obj
                    compileMethodCall' p' (objv++"->"++method) objv dt args
            Just (e, m') -> do
                ensureExtendIsDefined dt e
                compileMethodCall' m' ('_':pdt2str dt++'_':method) obj dt args

compileMethodCall' f n obj dt args = do
    let r = sreturnType f `ifDollar` dt
    argcodes <- checkargs (map snd (sparameters f)) args
    return (n++"("++joinComma (obj:argcodes)++")", r)

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

