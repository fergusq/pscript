-----------------------------------------------------------------------------
--
-- Module      :  Util
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

module Util where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.List

joinChar :: Char -> [String] -> String
joinChar _ [] = ""
joinChar _ [p] = p
joinChar c (p:ps) = p ++ (c:joinChar c ps)

joinColon :: [String] -> String
joinColon = joinChar ','

collapse :: (Ord k) => [(k, a)] -> Map.Map k [a]
collapse = foldr (\(k, v) m -> Map.insert k (v : fromMaybe [] (Map.lookup k m)) m) Map.empty

rec :: (Monad m) => ([a] -> m [[a]]) -> [a] -> m()
rec f [] = return ()
rec f a  = do v <- f a
              rec f (concat v)

firstJust :: [Maybe a] -> Maybe a
firstJust []           = Nothing
firstJust (Just a:as)  = Just a
firstJust (Nothing:as) = firstJust as

combinations :: (Eq a, Ord a) => [a] -> [[a]]
combinations xs = [1..length xs] >>= \n ->
    filter ((n==).length.nub) $ nub $ map sort $ mapM (const xs) [1..n]
