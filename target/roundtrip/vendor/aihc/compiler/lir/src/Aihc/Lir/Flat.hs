module Aihc.Lir.Flat (boxedArray, newInts, newBools, newWords, freezeInts, lengthOf, forUpTo, forDownFrom, forEach, addTo, scanSums, Rows, rowFrom, rowTo, rowAt, rowsSize, forRow, buildRows, IntBuffer, newIntBuffer, pushInt, bufferLength, freezeBufferWith, Table, newTable, openRow, pushValue, freezeTableWith, Bits, newBits, setBit, clearBit, clearRow, unionRow, replaceRow, forBits) where {
import Control.Monad (when);
import Control.Monad.ST (ST);
import Data.Array.Base (getNumElements);
import Data.Array.ST (STUArray, newArray, readArray, writeArray);
import Data.Array.Unboxed (Array, UArray, bounds, listArray, (!));
import Data.Array.Unsafe (unsafeFreeze);
import Data.Bits (complement, countTrailingZeros, shiftL, (.&.), (.|.));
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef);
import Data.Word (Word64);
boxedArray :: Int -> [e] -> Array Int e;
boxedArray size = listArray (0, size - 1);
newInts :: Int -> Int -> ST s (STUArray s Int Int);
newInts size = newArray (0, size - 1);
newBools :: Int -> Bool -> ST s (STUArray s Int Bool);
newBools size = newArray (0, size - 1);
newWords :: Int -> ST s (STUArray s Int Word64);
newWords size = newArray (0, size - 1) 0;
freezeInts :: STUArray s Int Int -> ST s (UArray Int Int);
freezeInts = unsafeFreeze;
lengthOf :: UArray Int Int -> Int;
lengthOf array = snd (bounds array) + 1;
forUpTo :: Int -> (Int -> ST s ()) -> ST s ();
forUpTo limit act = go 0 where {
  go index = when (index < limit) (act index >> go (index + 1))};
forDownFrom :: Int -> Int -> (Int -> ST s ()) -> ST s ();
forDownFrom highest lowest act = go highest where {
  go index = when (index >= lowest) (act index >> go (index - 1))};
forEach :: [a] -> (Int -> a -> ST s ()) -> ST s ();
forEach items act = go 0 items where {
  go _ [] = pure ();
  go position (item : rest) = act position item >> go (position + 1) rest};
addTo :: STUArray s Int Int -> Int -> Int -> ST s ();
addTo array index by = readArray array index >>= writeArray array index . (+ by);
scanSums :: STUArray s Int Int -> Int -> ST s ();
scanSums array count = forUpTo count (\index -> readArray array index >>= addTo array (index + 1));
data Rows = Rows !(UArray Int Int) !(UArray Int Int);
rowFrom, rowTo :: Rows -> Int -> Int;
rowFrom (Rows offsets _) index = offsets ! index;
rowTo (Rows offsets _) index = offsets ! (index + 1);
rowAt :: Rows -> Int -> Int;
rowAt (Rows _ values) at = values ! at;
rowsSize :: Rows -> Int;
rowsSize (Rows _ values) = lengthOf values;
forRow :: Rows -> Int -> (Int -> ST s ()) -> ST s ();
forRow rows index act = go (rowFrom rows index) where {
  go at = when (at < rowTo rows index) (act (rowAt rows at) >> go (at + 1))};
buildRows :: Int -> ((Int -> Int -> ST s ()) -> ST s ()) -> ST s Rows;
buildRows rowCount walk = do {
  offsets <- newInts (rowCount + 1) 0;
  walk (\row _ -> addTo offsets (row + 1) 1);
  scanSums offsets rowCount;
  cursor <- newInts (rowCount + 1) 0;
  forUpTo (rowCount + 1) (\index -> readArray offsets index >>= writeArray cursor index);
  total <- readArray offsets rowCount;
  values <- newInts total 0;
  walk $ \row value -> do {
  at <- readArray cursor row;
  writeArray cursor row (at + 1);
  writeArray values at value};
  Rows <$> freezeInts offsets <*> freezeInts values};
newtype IntBuffer s = IntBuffer (STRef s (STUArray s Int Int));
newIntBuffer :: Int -> ST s (IntBuffer s);
newIntBuffer capacity = IntBuffer <$> (newInts (max 4 capacity + 1) 0 >>= newSTRef);
pushInt :: IntBuffer s -> Int -> ST s ();
pushInt (IntBuffer arrayRef) value = do {
  array <- readSTRef arrayRef;
  count <- readArray array 0;
  capacity <- getNumElements array;
  target <- if count + 1 < capacity then pure array else do {
  bigger <- newInts (capacity * 2) 0;
  forUpTo capacity (\index -> readArray array index >>= writeArray bigger index);
  writeSTRef arrayRef bigger;
  pure bigger};
  writeArray target (count + 1) value;
  writeArray target 0 (count + 1)};
bufferLength :: IntBuffer s -> ST s Int;
bufferLength (IntBuffer arrayRef) = readSTRef arrayRef >>= (`readArray` 0);
freezeBufferWith :: (Int -> Int -> Int) -> IntBuffer s -> ST s (UArray Int Int);
freezeBufferWith rename buffer@(IntBuffer arrayRef) = do {
  count <- bufferLength buffer;
  array <- readSTRef arrayRef;
  result <- newInts count 0;
  forUpTo count (\index -> readArray array (index + 1) >>= writeArray result index . rename index);
  freezeInts result};
keep :: Int -> Int -> Int;
keep _ value = value;
data Table s = Table !(IntBuffer s) !(IntBuffer s);
newTable :: Int -> Int -> ST s (Table s);
newTable rowCount valueCount = Table <$> newIntBuffer rowCount <*> newIntBuffer valueCount;
openRow :: Table s -> ST s ();
openRow (Table offsets values) = bufferLength values >>= pushInt offsets;
pushValue :: Table s -> Int -> ST s ();
pushValue (Table _ values) = pushInt values;
freezeTableWith :: (Int -> Int) -> Table s -> ST s Rows;
freezeTableWith rename table@(Table offsets values) = do {
  openRow table;
  Rows <$> freezeBufferWith keep offsets <*> freezeBufferWith (const rename) values};
data Bits s = Bits !(STUArray s Int Word64) !Int;
newBits :: Int -> Int -> ST s (Bits s);
newBits rowCount valueCount = Bits <$> newWords (rowCount * stride) <*> pure stride where {
  stride = max 1 ((valueCount + 63) `div` 64)};
changeBit :: (Word64 -> Word64 -> Word64) -> Bits s -> Int -> Int -> ST s ();
changeBit change (Bits array stride) row value = do {
  let {
  index = row * stride + (value `div` 64)};
  word <- readArray array index;
  writeArray array index (change word (1 `shiftL` (value `mod` 64)))};
setBit, clearBit :: Bits s -> Int -> Int -> ST s ();
setBit = changeBit (.|.);
clearBit = changeBit (\word bit -> word .&. complement bit);
clearRow :: Bits s -> Int -> ST s ();
clearRow (Bits array stride) row = forUpTo stride (\word -> writeArray array (row * stride + word) 0);
unionRow :: Bits s -> Int -> Bits s -> Int -> ST s ();
unionRow (Bits into stride) row (Bits from _) fromRow = forUpTo stride $ \word -> do {
  there <- readArray from (fromRow * stride + word);
  here <- readArray into (row * stride + word);
  writeArray into (row * stride + word) (here .|. there)};
replaceRow :: Bits s -> Int -> Bits s -> Int -> ST s Bool;
replaceRow (Bits into stride) row (Bits from _) fromRow = go 0 False where {
  go word changed | word >= stride = pure changed | otherwise = do {
  there <- readArray from (fromRow * stride + word);
  here <- readArray into (row * stride + word);
  when (here /= there) (writeArray into (row * stride + word) there);
  go (word + 1) (changed || here /= there)}};
forBits :: Bits s -> Int -> (Int -> ST s ()) -> ST s ();
forBits (Bits array stride) row act = forUpTo stride $ \word -> do {
  bits <- readArray array (row * stride + word);
  values (word * 64) bits} where {
  values base bits | bits == 0 = pure () | otherwise = act (base + countTrailingZeros bits) >> values base (bits .&. (bits - 1))}
}
