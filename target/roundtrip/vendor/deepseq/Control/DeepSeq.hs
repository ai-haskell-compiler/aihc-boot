{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Control.DeepSeq (NFData(rnf), rwhnf) where {
import GHC.Generics;
class GNFData f where {
  grnf :: f a -> ()};
instance GNFData U1 where {
  grnf U1 = ()};
instance NFData a => GNFData (K1 i a) where {
  grnf = rnf . unK1};
instance GNFData a => GNFData (M1 i c a) where {
  grnf = grnf . unM1};
instance (GNFData a, GNFData b) => GNFData (a :*: b) where {
  grnf (x :*: y) = grnf x `seq` grnf y};
instance (GNFData a, GNFData b) => GNFData (a :+: b) where {
  grnf (L1 x) = grnf x;
  grnf (R1 x) = grnf x};
rwhnf :: a -> ();
rwhnf = (`seq` ());
class NFData a where {
  rnf :: a -> ();
  default rnf :: (Generic a, GNFData (Rep a)) => a -> ();
  rnf = grnf . from};
instance NFData Int where {
  rnf = rwhnf};
instance NFData Char where {
  rnf = rwhnf};
instance NFData a => NFData [a] where {
  rnf = foldr (\x r -> rnf x `seq` r) ()}
}
