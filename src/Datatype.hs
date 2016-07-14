module Datatype where

import Parser
import Util
import Data.List
import qualified Data.Map as Map

type PVariable = (String, PDatatype)

data PDatatype = PInterface String [PDatatype]
               | PSum [PDatatype]
               | PDollar
               | PNothing deriving (Eq, Ord)

-- PNothing on pseudotyyppi, jonka kääntäjä antaa virheellisille lausekkeille

pArray a = PInterface "Array" [a]
pPointer a = PInterface "Pointer" [a]
pInteger = PInterface "Int" []
pChar = PInterface "Char" []
pBool = PInterface "Bool" []
pString = PInterface "Str" []
pVoid = PInterface "Void" []

ifDollar :: PDatatype -> PDatatype -> PDatatype
ifDollar PDollar t           = t
ifDollar (PInterface n ts) t = PInterface n (map (`ifDollar` t) ts)
ifDollar PNothing _          = PNothing

isVarargsType :: PDatatype -> Bool
isVarargsType (PInterface "Func" _) = True
isVarargsType _                     = False

-- Muuttaa tietotyypin PDatatype-olioksi

dt2pdt :: Datatype -> PDatatype
dt2pdt (Typename name dts) = PInterface name (map dt2pdt dts)
dt2pdt (SumType dts) = PSum (map dt2pdt dts)
dt2pdt DollarType = PDollar

pdt2dt :: PDatatype -> Datatype
pdt2dt (PInterface name dts) = Typename name (map pdt2dt dts)
pdt2dt (PSum dts) = SumType (map pdt2dt dts)

ctype :: PDatatype -> String -> String
ctype (PInterface "Array" [a]) n = "struct _Array1" ++ pdt2str a ++ " " ++ n
ctype (PInterface "Pointer" [a]) n = ctype a ('*':n)
ctype (PInterface "Int" []) n = "int " ++ n
ctype (PInterface "Bool" []) n = "int " ++ n
ctype (PInterface "Char" []) n = "char " ++ n
ctype (PInterface "Str" []) n = "char*" ++ n
ctype (PInterface "Void" []) n = "char " ++ n
ctype PNothing n = "NOTHING " ++ n
ctype dt@(PInterface a ts) n = pdt2str dt ++ " " ++ n -- määritellään typedefinä
ctype dt@(PSum dts) n = "struct _" ++ pdt2str dt ++ ('*':n)

cparams :: [PDatatype] -> String
cparams [] = ""
cparams [a] = ctype a ""
cparams (a:as) = ctype a "" ++ ", " ++ cparams as

pdt2str :: PDatatype -> String
pdt2str (PInterface a []) = a
pdt2str (PInterface a as) = a ++ show (length as) ++ concatMap pdt2str as
pdt2str (PSum dts) = joinChar '_' (sort $ map pdt2str dts)
pdt2str PNothing = "<nothing>"

instance Show PDatatype where
    show (PInterface a []) = a
    show (PInterface a as) = a ++ "<" ++ joinComma (map show as) ++ ">"
    show (PSum dts) = joinChar '&' (sort $ map show dts)
    show PDollar = "$"
    show PNothing = "<nothing>"
