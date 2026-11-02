module Data.ByteString.Builder.Prim.Internal (FixedPrim, fixedPrim, contramapF, BoundedPrim, sizeBound, runB, toB, (>$<)) where {
import Foreign;
import Prelude hiding (maxBound);
class Contravariant f where {
  contramap :: (b -> a) -> f a -> f b};
(>$<) :: Contravariant f => (b -> a) -> f a -> f b;
(>$<) = contramap;
data FixedPrim a = FP !Int (a -> Ptr Word8 -> IO ());
fixedPrim :: Int -> (a -> Ptr Word8 -> IO ()) -> FixedPrim a;
fixedPrim = FP;
contramapF :: (b -> a) -> FixedPrim a -> FixedPrim b;
contramapF f (FP l io) = FP l (io . f);
toB :: FixedPrim a -> BoundedPrim a;
toB (FP l io) = BP l (\x op -> io x op >> (return $! op `plusPtr` l));
data BoundedPrim a = BP !Int (a -> Ptr Word8 -> IO (Ptr Word8));
sizeBound :: BoundedPrim a -> Int;
sizeBound (BP b _) = b;
runB :: BoundedPrim a -> a -> Ptr Word8 -> IO (Ptr Word8);
runB (BP _ io) = io;
instance Contravariant FixedPrim where {
  contramap = contramapF}
}
