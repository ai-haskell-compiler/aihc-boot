-- | Functions on Maybe.
module Data.Maybe
  ( Maybe (..),
    maybe,
    fromMaybe,
    fromJust,
    isJust,
    isNothing,
    catMaybes,
    mapMaybe,
    listToMaybe,
    maybeToList,
  )
where

import GHC.Base
import GHC.Maybe
import GHC.Prim
import GHC.Types

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

catMaybes :: [Maybe a] -> [a]
catMaybes ms = foldr (\m r -> maybe r (\x -> x : r) m) [] ms

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = foldr (\x r -> maybe r (\y -> y : r) (f x)) [] xs

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
