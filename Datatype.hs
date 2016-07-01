-----------------------------------------------------------------------------
--
-- Module      :  Datatype
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Datatype where

import Parser
import Util
import Data.List

type PVariable = (String, PDatatype)

data PDatatype = PInterface String [PDatatype]
               | PSum [PDatatype]
               | PDollar
               | PNothing deriving (Eq, Ord, Show)

-- PNothing on pseudotyyppi, jonka kääntäjä antaa virheellisille lausekkeille

pList a = PInterface "List" [a]
pFunction r ps = PInterface "Func" (r:ps)
pInteger = PInterface "Int" []
pBool = PInterface "Bool" []
pString = PInterface "Str" []
pVoid = PInterface "Void" []

ifDollar :: PDatatype -> PDatatype -> PDatatype
ifDollar PDollar t           = t
ifDollar (PInterface n ts) t = PInterface n (map (`ifDollar` t) ts)
ifDollar PNothing _          = PNothing

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
dt2pdt (SumType dts) = PSum (map dt2pdt dts)
dt2pdt DollarType = PDollar

ctype :: PDatatype -> String -> String
ctype (PInterface "List" [a]) n = "struct List1" ++ pdt2str a ++ " " ++ n
ctype (PInterface "Func" (r:ps)) n = ctype r ("(*" ++ n ++ ")(" ++ cparams ps ++ ")")
ctype (PInterface "Int" []) n = "int " ++ n
ctype (PInterface "Bool" []) n = "int " ++ n
ctype (PInterface "Str" []) n = "char*" ++ n
ctype (PInterface "Void" []) n = "char " ++ n
ctype PNothing n = "void*" ++ n
ctype (PInterface a _) n = "struct " ++ a ++ ('*':n)
ctype dt@(PSum dts) n = "struct " ++ pdt2str dt ++ ('*':n)

cparams :: [PDatatype] -> String
cparams [] = ""
cparams [a] = ctype a ""
cparams (a:as) = ctype a "" ++ ", " ++ cparams as

pdt2str :: PDatatype -> String
pdt2str (PInterface a []) = a
pdt2str (PInterface a as) = a ++ show (length as) ++ concatMap pdt2str as
pdt2str (PSum dts) = joinChar '_' (sort $ map pdt2str dts)
