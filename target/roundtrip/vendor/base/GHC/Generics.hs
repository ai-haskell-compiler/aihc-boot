{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module GHC.Generics (U1(..), K1(..), M1(..), (:+:)(..), (:*:)(..), Generic(..)) where {
import GHC.Types (Type);
infixr 5 :+:;
infixr 6 :*:;
data U1 p = U1;
newtype K1 i c p = K1 { unK1 :: c };
newtype M1 i c f p = M1 { unM1 :: f p };
data (:+:) f g p = L1 (f p) | R1 (g p);
data (:*:) f g p = f p :*: g p;
class Generic a where {
  type Rep a :: Type -> Type;
  from :: a -> Rep a x;
  to :: Rep a x -> a}
}
