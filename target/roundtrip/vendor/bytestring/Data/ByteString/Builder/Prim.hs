module Data.ByteString.Builder.Prim (primBounded, primMapListBounded, primFixed, primMapListFixed, char8) where {
import Data.ByteString.Builder.Internal;
import Data.Char (ord);
import Data.ByteString.Builder.Prim.Internal hiding (sizeBound);
import qualified Data.ByteString.Builder.Prim.Internal as I;
import Data.ByteString.Builder.Prim.Binary;
import Foreign;
primFixed :: FixedPrim a -> (a -> Builder);
primFixed = primBounded . toB;
primMapListFixed :: FixedPrim a -> ([a] -> Builder);
primMapListFixed = primMapListBounded . toB;
primBounded :: BoundedPrim a -> (a -> Builder);
primBounded w x = ensureFree (I.sizeBound w) `mappend` builder step where {
  step k (BufferRange op ope) = do {
  op' <- runB w x op;
  let {
  !br' = BufferRange op' ope};
  k br'}};
primMapListBounded :: BoundedPrim a -> [a] -> Builder;
primMapListBounded w xs0 = builder $ step xs0 where {
  step xs1 k (BufferRange op0 ope0) = go xs1 op0 where {
  go [] !op = k (BufferRange op ope0);
  go xs@(x' : xs') !op | op `plusPtr` bound <= ope0 = runB w x' op >>= go xs' | otherwise = return $ bufferFull bound op (step xs k)};
  bound = I.sizeBound w};
char8 :: FixedPrim Char;
char8 = (fromIntegral . ord) >$< word8
}
