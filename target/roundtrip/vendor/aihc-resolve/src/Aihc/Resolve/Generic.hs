{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aihc.Resolve.Generic (everywhereM, everything) where {
import Aihc.Parser.Syntax (SourceSpan);
import Data.Data (Data, gmapM, gmapQr);
import Data.Text (Text);
import Data.Typeable (Proxy(..), TypeRep, typeOf, typeRep);
everywhereM :: (Monad m, Data a) => (forall b. (Data b) => b -> m b) -> a -> m a;
everywhereM rewrite value | isLeaf value = pure value | otherwise = gmapM (everywhereM rewrite) value >>= rewrite;
everything :: forall a r. (Data a) => (forall b. (Data b) => b -> [r]) -> a -> [r];
everything query value = collect value [] where {
  collect :: forall b. (Data b) => b -> [r] -> [r];
  collect node rest | isLeaf node = rest | otherwise = query node ++ gmapQr ($) rest collect node};
isLeaf :: (Data a) => a -> Bool;
isLeaf value = rep == textTypeRep || rep == stringTypeRep || rep == sourceSpanTypeRep where {
  rep = typeOf value};
textTypeRep, stringTypeRep, sourceSpanTypeRep :: TypeRep;
textTypeRep = typeRep (Proxy :: Proxy Text);
stringTypeRep = typeRep (Proxy :: Proxy String);
sourceSpanTypeRep = typeRep (Proxy :: Proxy SourceSpan)
}
