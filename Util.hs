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

joinComma :: [String] -> String
joinComma = joinChar ','

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

search :: (Eq b) => (a -> b) -> [a] -> b -> Maybe a
search c ms m
    = let fms = filter (\m' -> c m' == m) ms
      in case fms of
            []    -> Nothing
            (a:_) -> Just a

combinations :: (Eq a, Ord a) => [a] -> [[a]]
combinations xs = nub $ map sort $ do n <- [1..length xs]
                                      let sublist = take (n-1) xs ++ drop n xs
                                      if null sublist
                                       then []
                                       else sublist:combinations sublist
