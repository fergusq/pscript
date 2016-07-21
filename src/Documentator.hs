module Documentator where

import Control.Monad.Writer
import Data.List (sort, sortBy)
import Data.Function (on)
import qualified Data.Map as Map

import Parser
import Compiler
import Util

escapeHtml :: String -> String
escapeHtml ('<':cs) = "&lt;" ++ escapeHtml cs
escapeHtml ('>':cs) = "&gt;" ++ escapeHtml cs
escapeHtml ('&':cs) = "&amp;" ++ escapeHtml cs
escapeHtml (  c:cs) = c : escapeHtml cs
escapeHtml []       = ""

eshow :: (Show s) => s -> String
eshow = escapeHtml . show

html = tell . (:[])

link' href code = "<a href=\""++href++"\">" ++ code ++ "</a>"
link href = html . link' href
anchor name code = html ("<a name=\""++name++"\">" ++ code ++ "</a>")

hdiv cl code = html ("<div class=\""++cl++"\">" ++ code ++ "</div>")

tag name = html . (("<"++name++">")++) . (++("</"++name++">"))

heading1 = tag "h1"
heading2 = tag "h2"
heading3 = tag "h3"
heading4 = tag "h4"

heading i = tag ("h"++show i)

tt = tag "tt"
pre = tag "pre"
b = tag "b"

paragraph = tag "p"

ulist = tag "ul"
li = tag "li"

long :: (String -> Writer [String] ()) -> Writer [String] () -> Writer [String] ()
long f g = f $ concatMap (' ':) s where (_, s) = runWriter g

documentList l es f = do
    documentList' l $ map ((\a->(a,a)).fst) es
    forM_ (map snd $ sortBy (compare `on` fst) es) f

documentList' l es =
    long ulist $ forM_ (sort es) $ \(a,n) -> long li $ link ("#"++l++"_"++a) n

documentTree :: String -> [Declaration] -> Writer [String] ()
documentTree moduleName decls = do
    html "<html>"
    html "<head><link rel=\"stylesheet\" href=\"style.css\"/></head>"
    html "<body>"
    let (functions, models, supers, structs, enums) = treeToLists decls
    heading1 ("Module " ++ moduleName)
    long (hdiv "functions") $ do
        heading2 "Functions"
        let fs = Map.toList functions
        documentList "f" fs $ documentFunc 3 supers
    long (hdiv "structs") $ do
        heading2 "Structs"
        let ss = Map.toList structs
        documentList "m" ss $ documentStruct 3 supers
    long (hdiv "enums") $ do
        heading2 "Enums"
        let es = Map.toList enums
        documentList "m" es $ documentEnum 3 supers
    long (hdiv "models") $ do
        heading2 "Models"
        let ms = Map.toList models
        documentList "m" ms $ documentModel 3 supers
    html "</body>"
    html "</html>"

showTps tps = if null tps then "" else "<" ++ joinComma (map ('@':) tps) ++ ">"

handleDocAnn as =
    case lookup "doc" as of
        Just [doc] -> paragraph doc
        _          -> return ()

f2str Function {
    name = name,
    funcTypeparameters = tps,
    parameters = ps,
    returnType = rt,
    annotations = as } = eshow rt
            ++ " " ++ name
            ++ showTps tps
            ++ "(" ++ joinComma (map (\(n, t) -> eshow t ++ " " ++ n) ps) ++ ");"

documentFunc lvl supers f@Function {
    name = name,
    funcTypeparameters = tps,
    parameters = ps,
    returnType = rt,
    annotations = as } = long (hdiv "function") $ do
        long (heading lvl) $ anchor ("f_"++name) name
        pre $ f2str f
        handleDocAnn as

nameValPair dt@(Typename t _) = (t,eshow dt)

documentExtensions lvl supers t =
    case Map.lookup t supers of
        Just es -> do
            heading lvl "Extensions"
            paragraph "This type is extended with:"
            documentList' "m" (sort $ map (nameValPair.model) es)
        Nothing -> return ()

documentModel lvl supers Model {
    modelName = mname,
    typeparameters = tps,
    prerequisites = prs,
    methods = ms,
    modelAnnotations = as } = long (hdiv "model") $ do
        long (heading lvl) $ anchor ("m_"++mname) $ mname ++ showTps tps
        unless (null prs) $ long paragraph $
            b ("Prerequisites: " ++
               joinComma (map ((\(n,m)->link' ("#m_"++n) m).nameValPair) prs))
        handleDocAnn as
        heading (lvl+1) "Definition"
        html "<pre>"
        html $ "model " ++ mname ++ showTps tps
        unless (null prs) $ html $ ": " ++ joinComma (map eshow prs)
        html "{\n"
        forM_ ms $ \m ->
            html $ "    " ++ f2str m ++ "\n"
        html "}\n</pre>"
        heading (lvl+1) "Methods"
        let fs = map (\f -> (name f, f)) ms
        documentList "fm" fs $ documentFunc (lvl+2) supers
        documentExtensions (lvl+1) supers mname

documentStruct lvl supers Struct {
    stcName = mname,
    stcTypeparameters = tps,
    stcFields = fs,
    isConst = ic,
    stcAnnotations = as
    } = long (hdiv "struct") $ do
        long (heading lvl) $ anchor ("s_"++mname) $ mname ++ showTps tps
        when ic $
            long paragraph $ b "This struct is declared constant (read-only)."
        handleDocAnn as
        heading (lvl+1) "Definition"
        html "<pre>"
        when ic $ html "const"
        html $ "struct " ++ mname ++ showTps tps ++ " {\n"
        forM_ fs $ \(n, t) ->
            html $ "    " ++ eshow t ++ " " ++ n ++ ";\n"
        html "}\n</pre>"
        documentExtensions (lvl+1) supers mname

documentEnum lvl supers EnumStruct {
    enmName = mname,
    enmTypeparameters = tps,
    enmCases = fs,
    enmAnnotations = as
    } = long (hdiv "enum") $ do
        long (heading lvl) $ anchor ("e_"++mname) $ mname ++ showTps tps
        handleDocAnn as
        heading (lvl+1) "Definition"
        html "<pre>"
        html $ "enum " ++ mname ++ showTps tps ++ " {\n"
        forM_ fs $ \(n, t) ->
            html $ "    " ++ n ++ "(" ++ joinComma (map eshow t) ++ ");\n"
        html "}\n</pre>"
        documentExtensions (lvl+1) supers mname
