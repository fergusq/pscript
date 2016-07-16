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
import Data.Char

joinChar :: Char -> [String] -> String
joinChar _ [] = ""
joinChar _ [p] = p
joinChar c (p:ps) = p ++ (c:joinChar c ps)

joinComma :: [String] -> String
joinComma [] = ""
joinComma [p] = p
joinComma (p:ps) = p ++ (", " ++ joinComma ps)

collapse :: (Ord k) => [(k, a)] -> Map.Map k [a]
collapse = foldr (\(k, v) m -> Map.insert k (v : fromMaybe [] (Map.lookup k m)) m) Map.empty

rec :: (Monad m) => ([a] -> m [[a]]) -> [a] -> m()
rec f [] = return ()
rec f a  = do v <- f a
              rec f (concat v)

rec2 :: (Monad m) => (a -> m ([a], [b])) -> [a] -> m [b]
rec2 f [] = return []
rec2 f as = do
    (as', bs) <- foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([], []) <$> forM as f
    bs' <- concat <$> forM as' (rec2 f)
    return (concat bs ++ bs')

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM = (. flip unless) . (>>=)

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

isIdentifierChar :: Char -> Bool
isIdentifierChar '_' = True
isIdentifierChar c = isAlphaNum c

forxM :: (Monad m) => [a] -> c -> (a -> c -> m (b, c)) -> m ([b], c)
forxM [] c callback = return ([], c)
forxM (a:as) c callback = do
    (b, c') <- callback a c
    (bs, c'') <- forxM as c' callback
    return (b:bs, c'')
