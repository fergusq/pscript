module Compiler where

import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Data.Maybe
import Data.Char
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
         lift . tell $ map (CodeFragment 500) [ctype rtype name', "{\n"]

generateIf :: String -> Compiler () -> Compiler ()
generateIf cond callback = do
    lift . tell $ map (CodeFragment 500) ["\tif(", cond, "){\n"]
    callback
    generateEnd

generateElseIf :: String -> Compiler () -> Compiler ()
generateElseIf cond callback = do
    lift . tell $ map (CodeFragment 500) ["\telse if(", cond, "){\n"]
    callback
    generateEnd

generateWhile :: String -> Compiler () -> Compiler ()
generateWhile cond callback = do
    lift . tell $ map (CodeFragment 500) ["\twhile(", cond, "){\n"]
    callback
    generateEnd

generateBreak :: Compiler ()
generateBreak =
    lift . tell $ map (CodeFragment 500) ["break;\n"]

generateContinue :: Compiler ()
generateContinue =
    lift . tell $ map (CodeFragment 500) ["continue;\n"]

generateElse :: Compiler  ()
generateElse
    = lift . tell $ map (CodeFragment 500) ["\t}else{\n"]

generateEnd :: Compiler ()
generateEnd
    = lift . tell $ map (CodeFragment 500) ["\t}\n"]

generateCreate :: PDatatype -> String -> String -> Compiler ()
generateCreate dt var value
    = do lift . tell $ map (CodeFragment 500) ["\t", ctype dt var,"=",value,";\n"]
         ensureStructIsDefined dt -- varmistetaan, että kaikki tyypin käsittelemiseen
                                  -- tarvittavat c-tietorakenteet ja funktiot on generoitu

generateVarDecl :: PDatatype -> String -> Compiler ()
generateVarDecl dt var
    = do lift . tell $ map (CodeFragment 500) ["\t", ctype dt var,";\n"]
         ensureStructIsDefined dt

generateAssign :: String -> String -> Compiler ()
generateAssign var value
    = lift . tell $ map (CodeFragment 500) ["\t",var,"=",value,";\n"]

generateReturn :: String -> Compiler ()
generateReturn value
    = lift . tell $ map (CodeFragment 500) ["\treturn ", value, ";\n"]

generateCode :: String -> Generator ()
generateCode code
    = tell [CodeFragment 500 code]

generateVarHeader :: PDatatype -> String -> Generator ()
generateVarHeader dt name
    = tell [CodeFragment 300 $ ctype dt name ++ ";\n"]

generateFunctionHeader :: PDatatype -> [PDatatype] -> String -> Generator ()
generateFunctionHeader r ps name
    = tell [CodeFragment 300 $ ctype r (name ++ "(" ++ cparams ps ++ ")") ++ ";\n"]

generateExternHeader :: PDatatype -> String -> Generator ()
generateExternHeader dt name
    = tell [CodeFragment 300 $ "extern " ++ ctype dt name ++ ";\n"]

generateExternFunctionHeader :: PDatatype -> [PDatatype] -> String -> Generator ()
generateExternFunctionHeader r ps name
    = tell [CodeFragment 300 $ "extern " ++ ctype r (name ++ "(" ++ cparams ps ++ ")") ++ ";\n"]

generateSubHeaderCode :: String -> Generator ()
generateSubHeaderCode code
    = tell [CodeFragment 400 code]

generateHeaderCode :: String -> Generator ()
generateHeaderCode code
    = tell [CodeFragment 300 code]

generateStructHeaderCode :: String -> Generator ()
generateStructHeaderCode code
    = tell [CodeFragment 200 code]

generateConstStructHeaderCode :: String -> Generator ()
generateConstStructHeaderCode code
    = tell [CodeFragment 150 code]

generateTypedefHeaderCode :: String -> Generator ()
generateTypedefHeaderCode code
    = tell [CodeFragment 100 code]

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
-- muunnos Int -> Float
checktype v f right@(PInterface "Float" []) (PInterface "Int" []) =
    generateCreate right v f
-- muunnos Int -> Long
checktype v f right@(PInterface "Long" []) (PInterface "Int" []) =
    generateCreate right v f
-- muunnos Float -> Int
checktype v f right@(PInterface "Int" []) (PInterface "Float" []) =
    generateCreate right v f
-- muunnis Char -> Int
checktype v f right@(PInterface "Int" []) (PInterface "Char" []) =
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
                                  (uncurry compileExpressionAs)

-- luo tarvittaessa uuden väliaikaismuuttujan, jotta sivuvaikutuksellist arvoa ei
-- suoritettaisi kahdesti
createTmpVarIfNeeded :: PDatatype -> String -> Compiler String
createTmpVarIfNeeded dt cexpr =
    if all isIdentifierChar cexpr then
        return cexpr
    else do
        var <- tmpVar
        generateCreate dt var cexpr
        return var

-- varmistaa, että tyypin vaatimat C-structit ja funktiot on luotu
ensureStructIsDefined :: PDatatype -> Compiler ()
ensureStructIsDefined dt =
    case dt of
        -- array tarvitsee oman structinsa
        PInterface "Array" [a] -> do
            let n = "_PS_Array1" ++ pdt2str a
            conditionallyCreateStruct n $ do
                lift $ generateTypedefHeaderCode
                    ("typedef struct " ++ n ++ " " ++ n ++ ";\n")
                lift $ generateConstStructHeaderCode
                    ("struct "++n++"{int len;" ++ ctype a "*ptr" ++ ";};\n")
        PInterface "Func" (rt:ps) -> do
            let n = pdt2str dt
            conditionallyCreateStruct n $ do
                lift $ generateTypedefHeaderCode
                    ("typedef struct " ++ n ++ " " ++ n ++ ";\n")
                lift $ generateConstStructHeaderCode
                    ("struct "++n++"{void* scope;"
                     ++ ctype rt "(*func)(" ++ joinComma ("void*":map (`ctype` "") ps)
                     ++ ");};\n")
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
            queueDeclConditionallyIfInList structs Stc scope n t dt
            -- jos tyyppi on enum, luodaan sille oma C-struct
            queueDeclConditionallyIfInList enums Enm scope n t dt
        -- intersektiotyypin struct on samanlainen kuin malliolion, mutta sen
        -- vtable sisältää kaikkien mallien metodit
        dt@(PSum dts) -> do
            let n = pdt2str dt
            conditionallyCreateStruct n $ do
                methods <- concat <$> mapM getModelMethods dts
                compileStruct dt n methods
        _   -> return ()

queueDeclConditionallyIfInList getList constr scope n t dt =
    let s = Map.lookup t $ getList scope
    in case s of
        Just s' ->
            conditionallyCreateStruct n $ do
                ss <- getSubstitutions dt
                queueDecl ss (constr s')
        _ -> return ()

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
    let tps = eTypeparameters extend
    if length tps /= length ts then do
        let severity = if isVarargsType dt then tellWarning else tellError
        severity ("invalid extension of generic type " ++ show dt ++ " with "
                  ++ show (model extend)
                  ++ ": wrong number of type parameters in extension declaration")
     else do
        let ss = Map.fromList $ zip (eTypeparameters extend) ts
        unless (isPNothing dt) $
            conditionallyCreateExtend (show dt ++ show (model extend)) $
                queueDecl ss $ Ext extend

-- varmistaa, että tyyppiparametrisoidun funktion haluttu versio generoidaan
ensureFunctionIsDefined :: [PDatatype] -> Function -> Compiler ()
ensureFunctionIsDefined ts func = unless (null $ funcTypeparameters func) $ do
    let ss = Map.fromList $ zip (funcTypeparameters func) ts
    let n = name func ++ "_" ++ joinChar '_' (map pdt2str ts)
    let displayName = name func ++ "<" ++ joinComma (map show ts) ++ ">"
    conditionallyCreateMethod n $
        queueDecl ss $ Func func {
            name = n,
            annotations = ("_Alias", [displayName]) : annotations func
        }

-- Pääfunktio

treeToLists decls =
    let functions = Map.fromList $
                concatMap (\d -> case d of
                    Func f -> [(name f, f)]
                    _      -> []) decls
        models = Map.fromList $
                concatMap (\d -> case d of
                    Mdl m -> [(modelName m, m)]
                    _     -> []) decls
        extends = concatMap (\d -> case d of
                    Ext e -> [(dtName e, e)]
                    _     -> []) decls
        supers = collapse extends

        listStruct = Struct "Array" ["T"] [
            ("len", Typename "Int" []),
            ("ptr", Typename "Pointer" [Typeparam "T"])
            ] True [] False

        structs = Map.fromList $ ("Array", listStruct) :
                concatMap (\d -> case d of
                    Stc s -> [(stcName s, s)]
                    _     -> []) decls

        enums = Map.fromList $ concatMap (\d -> case d of
                    Enm e -> [(enmName e, e)]
                    _     -> []) decls
    in (functions, models, supers, structs, enums)

compile :: [Declaration] -> Generator ()
compile decls = do
    let (functions, models, supers, structs, enums) = treeToLists decls
    generateStructHeaderCode "#include <stdlib.h>\n"
    generateStructHeaderCode "#include <gc.h>\n"
    generateStructHeaderCode "void * alloc(size_t x) { return GC_malloc(x); }\n"
    let scope = Scope {
      varscope = VarScope {
        functionName = "",
        variables = Map.empty,
        expectedReturnType = PNothing,
        subs = Map.empty,
        doesReturn = False,
        noWarnings = False
      },
      counter = 0,
      functions = functions,
      extends = supers,
      models = models,
      structs = structs,
      enums = enums,
      definedStructs = [],
      definedMethods = [],
      definedExtends = [],
      generatorQueue = [],
      declarationQueue = []
    }
    runStateT (do -- TODO siisti tämä
        rec (\ds -> do
            mapM_ compileDecl ds
            s@Scope { declarationQueue = q } <- get
            put s { declarationQueue = [] }
            return [q]) $ map (\a -> (Map.empty, a)) decls
        scope' <- get
        forM_ (generatorQueue scope') id
     ) scope
    return ()

-- Kääntää yksittäisen funktion

compileDecl :: (Subs, Declaration)
               -> Compiler ()
compileDecl (ss, ExIm str) =
    lift $ generateTypedefHeaderCode ("#include <" ++ str ++ ">\n")
compileDecl (ss, Func decl@Function { name = fname, parameters = params,
                                            returnType=rtype, body = Extern })
    = return () -- funktio pitäisi includoida extern import -komennolla
compileDecl (ss, Func func) =
    when (null (funcTypeparameters func) || not (null ss)) $ do
        let fname = name func
        -- käyttäjälle näytettävä nimi, jonka kääntäjä on liittänyt funktioon
        let nameToBeShown = case lookup "_Alias" $ annotations func of
                Just (n:_) -> n
                _ -> fname -- ei tarvitse validoida, koska vain
                           -- kääntäjä voi luoda annotaation
        -- substituoidaan parametreissa ja palautustyypeissä käytetyt tyyppiparametrit
        -- tyyppiargumenteilla
        params <- mapM (\(n,d) ->
            substitute' (Just $ "parameter type of "++fname) ss d
            >>= \d' -> return (n,d')) (parameters func)
        rtype <- substitute' (Just $ "return type of "++fname) ss $ returnType func
        -- varmistetaan, että tyyppien käyttämiseen tarvittavat c-structit luodaan
        forM_ (rtype:map snd params) $ \dt -> ensureStructIsDefined dt
        -- tarkistetaan, onko SuppressWarning -annotaatiota
        suppressWarnings <- case lookup "SuppressWarnings" $ annotations func of
                Just [] -> return True
                Just as  -> tellError ("wrong number of arguments for SuppressWarnings: "
                            ++ "expected 0, got "++show (length as))
                            >> return False
                Nothing -> return False
        -- alustetaan scope
        scope <- get
        let vscope = varscope scope
        put scope { varscope = vscope {
            functionName = nameToBeShown,
            variables = Map.fromList params,
            expectedReturnType = rtype,
            subs = ss,
            doesReturn = False,
            noWarnings = suppressWarnings
        }}
        -- varsinainen kääntäminen
        lift $ generateFunctionHeader rtype (map snd params) fname
        generateFunction rtype fname params
        compileStatement $ body func
        returns <- doesThisPathReturn
        when (rtype /= pVoid && not returns) $
            tellError "function does not return a value"
        generateEnd
compileDecl (ss, Mdl m) =
    when (null $ typeparameters m) $ do
        let mname = modelName m
        let dt = PInterface mname []
        fs <- mapM (subsFunction Map.empty) $ methods m
        conditionallyCreateStruct (pdt2str dt) $
            compileStruct dt (pdt2str dt) fs
compileDecl (ss, Ext Extend { dtName = n, model = m,
                                eMethods = fs, eTypeparameters = tps}) =
    when (null tps || not (null ss)) $ do
        dt <- substitute' (Just $ "extension declaration "++n++" with "++show m) ss m
        etas <- substituteTpList ("extend " ++ n) ss tps
        let edt = PInterface n etas
        prs <- map (`ifDollar` edt) <$> getPrerequisites dt
        es <- getSubstitutedExtends edt
        forM_ prs $ \prt ->
            unless (prt `elem` es) $
                tellError ("extension of " ++ show edt ++ " with " ++ show dt ++
                           " does not satisfy the interface: it does not implement " ++
                           show prt ++ ", which is a prerequisite")
        ms <- getModelMethods dt
        unless (length ms == length fs) $
            tellError ("extension of " ++ show edt ++ " with " ++ show dt ++
                       " does not satisfy the interface: there should be " ++
                       show (length ms) ++ " methods, not " ++ show (length fs))
        forM_ fs (\f -> do
            unless (null $ funcTypeparameters f) $
                tellError ("methods must not have typeparameters, but "
                           ++ name f ++ " has")
            mf' <- getModelMethod dt (name f)
            case mf' of
                Nothing -> tellError ("invalid extension of " ++ show edt ++ " with method "
                                      ++ name f ++ ": no such method in " ++ show dt)
                Just mf -> do
                    forM_ (zip (returnType f:map snd (parameters f))
                        (sreturnType mf:map snd (sparameters mf))) $
                        \(dt', mpdt') -> do
                            pdt <- substitute' (Just "extension method signature") ss dt'
                            let mpdt = mpdt' `ifDollar` edt
                            unless (pdt == mpdt) $
                                tellError ("extension method " ++ show edt ++ "." ++ name f ++
                                           " does not satisfy the interface defined in " ++
                                           show dt ++ ": expected " ++ show mpdt ++
                                           ", got " ++ show pdt)
                    queueDecl ss $ Func f {
                        name = '_' : pdt2str edt ++ "_" ++ name f,
                        parameters = ("this", Typename n (map Typeparam tps)) : parameters f,
                        annotations = ("_Alias", [show edt ++ '.' : name f])
                            : annotations f
                    }
         )
compileDecl (ss, Stc Struct { stcName = n, stcTypeparameters = tps, stcFields = fs,
                                isConst = c, isExtern = e }) =
    when (null tps || not (null ss)) $ do
        etas <- substituteTpList n ss tps
        let dt = PInterface n etas
        when e $ do
            lift $ generateTypedefHeaderCode ("typedef struct " ++ n ++
                                                 (if c then " " else "* ") ++ pdt2str dt ++ ";\n")
            lift $ generateTypedefHeaderCode ("typedef struct " ++ n ++ " _" ++ pdt2str dt ++ ";\n")
        unless e $ do
            lift $ generateTypedefHeaderCode ("typedef struct _" ++ pdt2str dt ++
                                                 (if c then " " else "* ") ++ pdt2str dt ++ ";\n")
            let generate = if c then generateConstStructHeaderCode else generateStructHeaderCode
            lift $ generate ("struct _" ++ pdt2str dt ++ "{\n")
            forM_ fs $ \(fname, ftype) -> do
                ftype' <- substitute' (Just $ "struct field "++show ftype++" "++fname) ss ftype
                lift $ generate ("\t" ++ ctype ftype' fname ++ ";\n")
            lift $ generate "};\n"
compileDecl (ss, Enm EnumStruct { enmName = n, enmTypeparameters = tps,
                                    enmCases = cs }) =
    when (null tps || not (null ss)) $ do
        etas <- substituteTpList n ss tps
        let dt = PInterface n etas
        lift $ generateStructHeaderCode ("enum e_" ++ pdt2str dt ++ " {\n")
        forM_ cs $ \(cname, _) ->
            lift $ generateStructHeaderCode ("\t" ++ pdt2str dt ++ "_" ++ cname ++ ",\n")
        lift $ generateStructHeaderCode "};\n"
        lift $ generateTypedefHeaderCode ("typedef struct _" ++ pdt2str dt ++ " "
                                             ++ pdt2str dt ++ ";\n")
        lift $ generateStructHeaderCode ("struct _" ++ pdt2str dt ++ "{\n")
        lift $ generateStructHeaderCode ("\tenum e_" ++ pdt2str dt ++ " _type;\n")
        lift $ generateStructHeaderCode "\tunion {\n"
        forM_ cs $ \(cname, ftypes) -> do
            lift $ generateStructHeaderCode "\t\tstruct {\n"
            forM_ (zip [1..] ftypes) $ \(i, ftype) -> do
                ftype' <- substitute' (Just $ "type of enum case field "++cname++":"++show i)
                    ss ftype
                lift $ generateStructHeaderCode ("\t\t\t" ++ ctype ftype' ("_k"++show i)
                                                ++ ";\n")
            lift $ generateStructHeaderCode ("\t\t} "++cname++";\n")
        lift $ generateStructHeaderCode "\t};\n};\n"

substituteTpList :: String -> Subs -> [String] -> Compiler [PDatatype]
substituteTpList n ss tps = case forM tps (`Map.lookup` ss) of
    Just etas -> return etas
    Nothing -> do
        tellError ("instance of declaration "
                   ++ n ++ "<" ++ joinComma (map ('@':) tps) ++ ">" ++
                   " has unknown type parameters")
        if Map.null ss then tellNote "no known type parameter substitutions" else do
            tellNote "known type parameter substitutions are:"
            forM_ (Map.toList ss) $ \(n, dt) ->
                tellNote ('@':n ++ " = " ++ show dt)
        return []

-- generoi malli- tai intersektiotyypin structin
compileStruct :: PDatatype -> String -> [FSignature] -> Compiler ()
compileStruct dt mname methods = do
    lift $ generateTypedefHeaderCode ("typedef struct _" ++ mname ++ "* " ++
                                         mname ++ ";\n")
    lift $ generateStructHeaderCode ("struct _" ++ mname ++ "{\n")
    -- generoidaan vtable
    forM_ methods $ \f ->
        let r = sreturnType f `ifDollar` dt
            ps = dt : map snd (sparameters f)
        in lift $ generateStructHeaderCode ("\t" ++
            ctype r ('(':'*':sname f ++ ")(" ++ cparams ps ++ ")") ++ ";\n")
    -- generoidaan kombinaatioiden oliot
    case dt of
        PSum dts -> forM_ (combinations dts) $ \c ->
            lift $ generateStructHeaderCode ("\t" ++ ctype (sumOrModel c) (concatMap pdt2str c)
                                       ++ ";\n")
        _ -> return ()
    lift $ generateStructHeaderCode "\tvoid *_obj;\n};\n"

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
compileStatement Break =
    generateBreak
compileStatement Continue =
    generateContinue
compileStatement (For name (Range from to) body)
    = do fromv <- compileExpressionAs pInteger from
         tov' <- compileExpressionAs pInteger to
         tov <- createTmpVarIfNeeded pInteger tov'
         generateCreate pInteger name fromv
         generateWhile (name++"<="++tov) $ do
             scope <- saveScope
             putVar name pInteger
             compileStatement body
             generateAssign name (name ++ "+1")
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
compileStatement (Expr (Call name tas args)) = do
    (_, code) <- compileCall pVoid name tas args
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
compileStatement (Match expr cases) = do
    (exprv', dt) <- compileExpression PNothing expr
    exprv <- createTmpVarIfNeeded dt exprv'
    forM_ (zip [1..] cases) $ \(i, (mcond, body)) -> do
        (cond, assigns) <- compileMatchCase exprv dt mcond
        (if i == 1 then generateIf else generateElseIf) cond $ do
            scope <- saveScope
            forM_ assigns $ \(varname, vardt, value) -> do
                generateCreate vardt varname value
                putVar varname vardt
            compileStatement body
            restoreScope scope

-- generoi ehtolauseen, jonka ollessa tosi patterni matchaa arvoon, sekä joukon
-- muuttujasijoituksia (nimi, tyyppi, arvo)
compileMatchCase :: String -> PDatatype -> MatchCondition ->
                    Compiler (String, [(String, PDatatype, String)])
compileMatchCase var dt mcond@(MatchCond caseName fieldMatches) = do
    cs' <- getCases dt
    case cs' of
        Just cs ->
            case lookup caseName cs of
                Just fs -> do
                    let cond = var++"._type=="++pdt2str dt++"_"++caseName
                    let fsWithNames = zip (map (\i->"_k"++show i) [1..]) fs
                    (conds, assigns) <- foldr (\(a, b) (as,bs) -> (a++"&&"++as,b:bs)) ("1",[])
                        <$> forM (zip fsWithNames fieldMatches)
                            (\((n,dt), fm) ->
                                compileMatchCase (var++"."++caseName++"."++n) dt fm)
                    return (cond++"&&"++conds, concat assigns)
                Nothing ->
                    -- jos ei matchata casea vastaan, kyseessä on muuttuja, johon
                    -- vain sijoitetaan arvo
                    return ("1", [(caseName, dt, var)])
        Nothing ->
            if not (null fieldMatches) then do
                tellError ("attempted to match a non-enum type " ++ show dt)
                return ("NOTHING", [])
            else
                -- jos ei matchata enumia vastaan, muuttuja on ainoa mahdollinen match
                return ("1", [(caseName, dt, var)])
compileMatchCase var dt mcond@(MatchStr str) =
    case dt of
        PInterface "Str" [] ->
            return ("strcmp("++var++", "++show str++")==0", [])
        _ -> do
            tellError ("attempted to match a non-str type " ++ show dt)
            return ("NOTHING", [])
compileMatchCase var dt mcond@(MatchInt int) =
    case dt of
        PInterface "Int" [] ->
            return (var++"=="++show int, [])
        _ -> do
            tellError ("attempted to match a non-int type " ++ show dt)
            return ("NOTHING", [])

compileCall expdt name tas' args = do
    ss <- getCurrentSubs
    tas <- mapM (substitute' (Just "function type arguments") ss) tas'
    f' <- getFunction name
    case f' of
        Just f -> let tps = funcTypeparameters f
            in if null tps
            then do
                -- käännetään tyyppiparametrisoimaton funktio
                let rt = dt2pdt $ returnType f
                let ps = map (dt2pdt.snd) $ parameters f
                argcodes <- checkargs ps args
                return (rt, name++"("++joinComma argcodes++")")
            else if null tas
            then do
                let rt' = returnType f
                let ps' = map snd $ parameters f

                -- alustava inferointi odotetun tyypin perusteella
                rtss <- Map.fromList <$> matchTypeArguments rt' expdt

                -- apufunktio, joka tekee toiminnon argumenteille ja inferoi samalla
                -- tyyppejä
                let inferUsing oss func = forxM (zip ps' args) oss $
                        \(par', arg) ss -> do
                            pt <- substitute' Nothing ss par'
                            (acode, atype) <- func pt arg
                            ss' <- Map.fromList <$>
                                matchTypeArguments par' atype
                            return ((acode, atype), ss `noNothingUnion` ss')

                -- "ennustetaan" tyyppiargumenttien arvot käyttämällä parametrityyppien
                -- ennusteita
                (_, pss) <- inferUsing rtss $
                    \_ a -> predictExprType a >>= \t -> return ((), t)

                -- käännetään argumentit ja inferoidaan samalla
                (acts, ss) <- inferUsing pss compileExpression

                -- selvitetään lopulliset tyypit inferoinnin perusteella
                rt <- substitute' (Just $ "return type of called " ++ name) ss rt'
                ps <- mapM (substitute' (Just $ "parameter type of called " ++ name) ss) ps'

                -- tehdään argumenttien vaatimat tyyppimuunnokset
                argcodes <- forM (zip ps acts) $ uncurry $ \pt (argcode', argtype') -> do
                    var <- tmpVar
                    argv <- createTmpVarIfNeeded argtype' argcode'
                    checktype var argv pt argtype'
                    return var

                -- muodostetaan lista inferoiduista tyyppiargumenteista
                tas <- forM tps $ \tp ->
                    case Map.lookup tp ss of
                        Nothing -> do
                            tellError ("can't infer value of @" ++ tp)
                            return PNothing
                        Just PNothing -> do
                            tellError ("can't infer value of @" ++ tp)
                            return PNothing
                        Just t ->
                            return t

                -- varmistetaan, että oikea versio funktiosta luodaan
                ensureFunctionIsDefined tas f

                return (rt, name++"_"++joinChar '_' (map pdt2str tas)
                        ++"("++joinComma argcodes++")")
            else do
                -- varmistetaan, että oikea versio funktiosta luodaan
                ensureFunctionIsDefined tas f
                -- substituoidaan tyyppiargumentit
                let fss = Map.fromList $ zip tps tas
                let rt' = returnType f
                let ps' = map snd $ parameters f
                rt <- substitute' (Just $ "return type of called " ++ name) fss rt'
                ps <- mapM (substitute' (Just $ "parameter type of called " ++ name) fss) ps'

                argcodes <- checkargs ps args
                return (rt, name++"_"++joinChar '_' (map pdt2str tas)
                        ++"("++joinComma argcodes++")")
        Nothing -> do
            tellError ("function not found: `" ++ name ++ "'")
            return (PNothing, "NOTHING")

matchTypeArguments :: Datatype -> PDatatype -> Compiler [(String, PDatatype)]
matchTypeArguments par@(Typename pname pts) arg@(PInterface aname ats) =
    if pname == aname
        then concat <$> forM (zip pts ats) (uncurry matchTypeArguments)
        else return []
matchTypeArguments _ PNothing = return []
matchTypeArguments par@(Typeparam pname) arg = return [(pname, arg)]
matchTypeArguments _ _ = return []

noNothingUnion :: Subs -> Subs -> Subs
ss `noNothingUnion` ss' = ss `nnunion` Map.toList ss'

ss `nnunion` [] = ss
ss `nnunion` ((n,t):ss') = case Map.lookup n ss of
    Nothing -> Map.insert n t ss `nnunion` ss'
    Just dt -> (if isPNothing dt then Map.insert n t ss else ss) `nnunion` ss'

-- Lausekkeiden kääntäjät

predictExprType :: Expression -> Compiler PDatatype
predictExprType (Int _) = return pInteger
predictExprType (Str _) = return pString
predictExprType (Cast dt _) = getCurrentSubs >>= \ss -> substitute' Nothing ss dt
predictExprType EmptyList = return $ pArray PNothing
predictExprType (List (expr:_)) = pArray <$> predictExprType expr
predictExprType (Range _ _) = return $ pArray pInteger
predictExprType (NewList dt _)
    = getCurrentSubs >>= \ss -> pArray <$> substitute' Nothing ss dt
predictExprType (NewPtrList dt _)
    = getCurrentSubs >>= \ss -> pPointer <$> substitute' Nothing ss dt
predictExprType (NewStruct dt _) = getCurrentSubs >>= \ss -> substitute' Nothing ss dt
predictExprType (NewEnumStruct dt _ _) = getCurrentSubs >>= \ss -> substitute' Nothing ss dt
predictExprType (Lambda ps rt _ )
    = PInterface "Func" <$> (getCurrentSubs >>=
        \ss -> mapM (substitute' Nothing ss) (rt : map snd ps))
predictExprType (Var name)
    = getVar name >>= \m -> case m of { Just dt -> return dt; Nothing -> return PNothing }
predictExprType _ = return PNothing

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
compileExpression expdt (Call name tas args) = do
    (retType, code) <- compileCall expdt name tas args
    return (code, retType)
compileExpression expdt (List (expr:exprs)) = do
    var <- tmpVar
    (firstv, dt) <- compileExpression PNothing expr -- Käännetään ensimmäisen alkion koodi
                                                    -- tyypin selvittämiseksi
    generateCreate (pArray dt) var ('{': show (length exprs + 1) ++ ", alloc("
                              ++ show (length exprs + 1)
                              ++ "*sizeof(" ++ ctype dt "" ++ "))}")
    generateAssign (var++".ptr[0]") firstv
    mapM_ (\(i,val) -> do valuecode <- compileExpressionAs dt val
                          generateAssign (var ++ ".ptr["++show i++"]") valuecode
       ) (zip [1..length exprs] exprs)
    return (var, pArray dt)
compileExpression expdt EmptyList =
    case expdt of
        PInterface "Array" [dt] -> do
            var <- tmpVar
            generateCreate (pArray dt) var "{0, 0}"
            return (var, pArray dt)
        _ -> do
            tellError "can't infer the type of empty list"
            tellNote ("expected type = " ++ show expdt)
            return ("NOTHING", pArray PNothing)
compileExpression expdt (Range from to) = do
    var <- tmpVar
    fromv' <- compileExpressionAs pInteger from
    fromv <- createTmpVarIfNeeded pInteger fromv'
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
    pdt <- substitute' (Just "list constructor") ss dt
    sizev' <- compileExpressionAs pInteger size
    sizev <- createTmpVarIfNeeded pInteger sizev'
    generateCreate (pArray pdt) var ('{': sizev ++ ", alloc("
                                     ++ sizev
                                     ++ "*sizeof(" ++ ctype pdt "" ++ "))}")
    return (var, pArray pdt)
compileExpression expdt (NewStruct dt fieldValues) = do
    ss <- getCurrentSubs
    pdt <- substitute' (Just "struct constructor") ss dt
    fs' <- getFields pdt
    case fs' of
        Just fs -> do
            var <- tmpVar
            fieldvaluecodes <- checkargs (map snd fs) fieldValues
            (Just cons) <- isConstant pdt
            (Just ext) <- isExternal pdt
            if not cons
             then do
                if not ext
                 then generateCreate pdt var ("alloc(sizeof(struct _" ++ pdt2str pdt ++ "))")
                 else generateCreate pdt var ("alloc(sizeof(_" ++ pdt2str pdt ++ "))") -- _-etuliitteiden versio on typedef varsinaiseen ulkoiseen structiin
                forM_ (zip fs fieldvaluecodes) $ \((n, _), c) ->
                    generateAssign (var++"->"++n) c
             else
                generateCreate pdt var ("{" ++ joinComma fieldvaluecodes ++ "}")
            return (var, pdt)
        Nothing -> do
            tellError ("struct not found: " ++ show dt)
            return ("NOTHING", PNothing)
compileExpression expdt (NewEnumStruct dt caseName fieldValues) = do
    ss <- getCurrentSubs
    pdt <- substitute' (Just "enum case constructor") ss dt
    cs' <- getCases pdt
    case cs' of
        Just cs -> do
            let fs' = lookup caseName cs
            case fs' of
                Just fs -> do
                    var <- tmpVar
                    fieldvaluecodes <- checkargs fs fieldValues
                    generateVarDecl pdt var
                    generateAssign (var++"._type") (pdt2str pdt ++ "_" ++ caseName)
                    forM_ (zip [1..] fieldvaluecodes) $ \(i, c) ->
                        generateAssign (var++"."++caseName++"._k"++show i) c
                    return (var, pdt)
                Nothing -> do
                    tellError ("enum case not found: " ++ show dt ++ "::" ++ caseName)
                    return ("NOTHING", PNothing)
        Nothing -> do
            tellError ("enum not found: " ++ show dt)
            return ("NOTHING", PNothing)
compileExpression expdt (NewPtrList dt size) = do
    var <- tmpVar
    ss <- getCurrentSubs
    pdt <- substitute' (Just "pointer allocation") ss dt
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
        (valcode, dt2) <- compileExpression t val
        valv <- createTmpVarIfNeeded dt2 valcode
        var3 <- tmpVar
        checktype var3 valv t dt2
        (Just constant) <- isConstant dt
        when constant $
            tellError ("trying to modify a const struct "++show dt)
        generateAssign (var++"->"++field) var3
        return (valv, dt2)
compileExpression expdt (Cast dt expr) = do
    ss <- getCurrentSubs
    pdt <- substitute' (Just $ "cast to " ++ show dt) ss dt
    var <- compileExpressionAs pdt expr
    return (var, pdt)
compileExpression expdt (Ref name) =
    case expdt of
        PInterface "Func" (rt':ps') -> do
            ps <- forM ps' $ \pt -> do
                var <- tmpVar
                return (var, pdt2dt pt)
            let rt = pdt2dt rt'
            -- purkkaa
            compileExpression expdt (Lambda ps rt $ Return $ Call name [] $ map (Var . fst) ps)
        _ -> do
            tellError ("can't infer the type of function reference &" ++ name)
            return ("NOTHING", PNothing)
compileExpression expdt (Lambda ps'' rt'' stmt) = do
    -- tarkistetaan, että parametrit eivät peitä muuttujia
    forM_ ps'' $ \(n, _) -> do
        v <- getVar n
        when (isJust v) $ tellError ("lambda parameter `" ++ n
                                     ++ "' shadows an existing variable")
    -- inferoidaan parametrien ja paluuarvon tyypit odotetusta tyypistä
    let expOrImp AutoType t = t
        expOrImp t        _ = t
    let (ps', rt') = case expdt of
            PInterface "Func" (ert:eps) ->
                (map (\((str, dt), pdt) -> (str, expOrImp dt $ pdt2dt pdt)) $ zip ps'' eps, expOrImp rt'' $ pdt2dt ert)
            _ -> (ps'', rt'')
    forM_ (("", rt') : ps') $ \(_, dt) ->
        case dt of
            AutoType -> do
                let str = show $ Typename "Func" (rt' : map snd ps')
                tellError("cannot infer lambda type (result: " ++ str ++ ", hint: " ++ show expdt ++ ")")
            _ -> return ()
    -- selvitetään muuttujat, jotka lambda kaappaa
    scope <- get
    let currentVariables = Map.toList $ variables $ varscope scope
    -- luodaan nimi lambdafunktiolle
    num <- nextNum
    let fn = "_lambda" ++ show num
        sn = fn ++ "_scope"
        wn = fn ++ "_wrapper"
    -- lisätään varsinainen lambdafunktio jonoon
    ss <- getCurrentSubs
    currentFName <- getCurrentFunctionName
    queueDecl ss $ Func $
        Function fn [] (map (second pdt2dt) currentVariables++ps') rt' stmt
        [("_Alias", ["lambda:"++show num++" in "++currentFName])]
    -- tehdään structi kaapattavien muuttujien säilömiseen
    lift $ generateHeaderCode ("struct "++sn++" {\n")
    forM_ currentVariables $ \(n, dt) ->
        lift $ generateHeaderCode ("\t" ++ ctype dt n ++ ";\n")
    lift $ generateHeaderCode "};\n"
    -- tehdään wrapperifunktio, joka lukee structista muuttujien arvot ja syöttää ne
    -- varsinaiselle funktiolle
    rt <- substitute' (Just "return type of lambda function") ss rt'
    ps <- mapM (\(n, dt') -> do
        dt <- substitute' (Just "parameter type of lambda function") ss dt'
        return (n, dt)) ps'
    let parcode (n,dt) = ctype dt n
    lift $ generateSubHeaderCode (ctype rt
        (wn ++ "("++ joinComma
            ("void *_scopep":map parcode ps)
         ++ ")") ++ "{\n")
    lift $ generateSubHeaderCode ("\tstruct " ++ sn ++ " *_scope = (struct "
                               ++ sn ++ "*) _scopep;\n")
    let argcodes = map (("_scope->" ++).fst) currentVariables ++ map fst ps
    lift $ generateSubHeaderCode ("\treturn " ++ fn ++ "(" ++ joinComma argcodes ++ ");\n};\n")
    -- luodaan nykyisistä muuttujista olio
    scopev <- tmpVar
    generateAssign ("struct " ++ sn ++ " *" ++ scopev) ("alloc(sizeof(struct "++sn++"))")
    forM_ currentVariables $ \(n, dt) ->
        generateAssign (scopev++"->"++n) n
    -- luodaan olio, joka sisältää pointterin muuttujastructiin ja wrapperifunktioon
    let dt = PInterface "Func" (rt:map snd ps)
    ensureStructIsDefined dt
    var <- tmpVar
    generateCreate dt var ("{"++scopev++","++wn++"}")
    return (var, dt)
compileExpression expdt TrueConstant = return ("1", pBool)
compileExpression expdt FalseConstant = return ("0", pBool)
compileExpression expdt (MethodCall obj method args)
    = do (objv, dt) <- compileExpression PNothing obj
         compileMethodCall dt objv method args

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

type MethodCallCompiler = PDatatype -> String
                     -> String -> [Expression]
                     -> Compiler (String, PDatatype)

compileIntLikeMethodCall :: MethodCallCompiler

compileIntLikeMethodCall objdt obj method args
    | method == "op_add" || method == "op_sub" || method == "op_mul" || method == "op_div"
        || method == "op_mod"
        = do (var:vars) <- checkargs [objdt] args
             return ("("++obj++coperator method++var++")", objdt)
    | method == "op_eq" || method == "op_neq" || method == "op_lt" || method == "op_gt"
        || method == "op_le" || method == "op_ge"
        = do (var:vars) <- checkargs [objdt] args
             return ("("++obj++coperator method++var++")", pBool)
    | method == "op_neg"
        = do checkargs [] args
             return ("-("++obj++")", objdt)
    | otherwise
        = compileOtherMethodCall objdt obj method args

compileMethodCall :: MethodCallCompiler

-- PInteger
compileMethodCall dt@(PInterface "Int" []) obj method args
    = compileIntLikeMethodCall dt obj method args

-- PLong
compileMethodCall dt@(PInterface "Long" []) obj method args
    = compileIntLikeMethodCall dt obj method args

-- PFloat
compileMethodCall dt@(PInterface "Float" []) obj method args
    = compileIntLikeMethodCall dt obj method args

-- PChar
compileMethodCall dt@(PInterface "Char" []) obj method args
    = compileIntLikeMethodCall dt obj method args

-- PBool
compileMethodCall (PInterface "Bool" []) obj method args
    | method == "op_and" || method == "op_or" || method == "op_eq" || method == "op_neq"
        = do (var:vars) <- checkargs [pBool] args
             return ("("++obj++coperator method++var++")", pBool)
    | method == "op_not"
        = do checkargs [] args
             return ("!("++obj++")", pBool)

-- PVoid
compileMethodCall (PInterface "Void" []) obj method args
    | method == "op_eq"
        = do (var:vars) <- checkargs [pVoid] args
             return ("1", pBool)
    | method == "op_neq"
        = do (var:vars) <- checkargs [pVoid] args
             return ("0", pBool)

-- PArray
compileMethodCall (PInterface "Array" [dt]) obj method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             return (obj ++ ".ptr["++index++"]", dt)
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             v <- createTmpVarIfNeeded dt value
             generateAssign (obj ++ ".ptr["++index++"]") v
             return (v, dt)

-- PPointer
compileMethodCall (PInterface "Pointer" [dt]) obj method args
    | method == "op_get"
        = do (index:vars) <- checkargs [pInteger] args
             tellWarning "pointer arithmetic is not typesafe"
             return (obj ++ "["++index++"]", dt)
    | method == "op_set"
        = do (index:value:vars) <- checkargs [pInteger, dt] args
             v <- createTmpVarIfNeeded dt value
             generateAssign (obj ++ "["++index++"]") v
             tellWarning "pointer arithmetic is not typesafe"
             return (v, dt)

-- PPointer
compileMethodCall dt@(PInterface "Func" (rt:ps)) obj method args
    | method == "call"
        = do vars <- checkargs ps args
             objv <- createTmpVarIfNeeded dt obj
             return (objv ++ ".func("++joinComma ((objv++".scope"):vars)++")", rt)

-- Määrittelemättömät metodit
compileMethodCall dt@(PSum dts) obj method args
    = do ms <- mapM (\dt -> do f <- getModelMethod dt method;
                               return $ do {f' <- f; return (dt, f') } ) dts
         let m = firstJust ms
         case m of
            Nothing -> do
                tellError ("type " ++ show dt ++ " does not have method `" ++ method ++ "'")
                return ("NOTHING", PNothing)
            Just (dt', m') -> do
                objv <- createTmpVarIfNeeded dt obj
                compileMethodCall' m' (objv++"->"++method) objv dt args
compileMethodCall dt obj method args
    = compileOtherMethodCall dt obj method args

compileOtherMethodCall :: MethodCallCompiler
compileOtherMethodCall dt obj method args
    = do m' <- getDTypeMethod dt method
         case m' of
            Nothing -> do
                p' <- getModelMethod dt method
                case p' of
                  Nothing ->
                    if method == "op_eq" || method == "op_neq"
                     then do
                       (var:vars) <- checkargs [dt] args
                       return ("("++obj++coperator method++var++")", pBool)
                     else do
                       tellError ("type " ++ show dt ++
                                  " does not have method `" ++ method ++ "'")
                       return ("NOTHING", PNothing)
                  Just p -> do
                    objv <- createTmpVarIfNeeded dt obj
                    compileMethodCall' p (objv++"->"++method) objv dt args
            Just (e, m) -> do
                ensureExtendIsDefined dt e
                compileMethodCall' m ('_':pdt2str dt++'_':method) obj dt args

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

