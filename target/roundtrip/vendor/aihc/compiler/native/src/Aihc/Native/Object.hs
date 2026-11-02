{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Aihc.Native.Object (Object, newObject, assembleObject, applyAll, currentSectionRole, selectSection, addGlobal, Item(..), Fixup(..), emitItem, emitItems, emitAlign, sealFunction, layoutObject, FixupKind(..), Image(..), ImageSection(..), imageMetadata, Name(..), nameText, ObjectError(..), Relocation(..), SectionRole(..), Symbol(..)) where {
import Control.Monad (filterM, when);
import Control.Monad.ST (ST, runST);
import Data.Array (listArray, (!));
import Data.Bits (shiftL, shiftR, (.&.), (.|.));
import Data.ByteString (ByteString);
import Data.ByteString qualified as BS;
import Data.ByteString.Lazy qualified as BL;
import Data.ByteString.Short (toShort);
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort);
import Data.Int (Int64, Int8);
import Data.IntMap.Strict qualified as IntMap;
import Data.List (sortOn);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray, copyByteArray, newPinnedByteArray, readByteArray, unsafeFreezeByteArray, writeByteArray);
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar);
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, setPrimArray, writePrimArray);
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Vector.Generic.Mutable qualified as MG;
import Data.Vector.Mutable qualified as MV;
import Data.Vector.Unboxed.Mutable qualified as MU;
import Data.Word (Word32, Word64, Word8);
data SectionRole = TextSection | TextConstantsSection | ReadOnlySection | DataSection | NoExecuteStackSection deriving (Enum, Eq, Ord, Show);
data FixupKind = Arm64Branch26 | Arm64Branch19 | Arm64Adr21 | Arm64Page21 | Arm64PageOffset12 | Absolute64 | X86Pc32 | X86Plt32 deriving (Enum, Eq, Show);
data Name = SymbolName !Text | LocalName !Int Text;
instance Eq Name where {
  SymbolName left == SymbolName right = left == right;
  LocalName left _ == LocalName right _ = left == right;
  _ == _ = False};
instance Ord Name where {
  compare (LocalName left _) (LocalName right _) = compare left right;
  compare (LocalName _ _) (SymbolName _) = LT;
  compare (SymbolName _) (LocalName _ _) = GT;
  compare (SymbolName left) (SymbolName right) = compare left right};
instance Show Name where {
  showsPrec precedence name = showsPrec precedence (nameText name)};
nameText :: Name -> Text;
nameText name = case name of {
  SymbolName text -> text;
  LocalName _ text -> text};
data Fixup = Fixup { fixupKind :: !FixupKind, fixupTarget :: !Name, fixupAddend :: !Int64, fixupWidth :: !Int, fixupWord :: !Word64 } deriving (Eq, Show);
data Item = Bytes !ByteString | Word !Int !Word64 | Label !Name | Apply !Fixup deriving (Eq, Show);
data Symbol = Symbol { symbolName :: !Text, symbolGlobal :: !Bool, symbolSection :: !(Maybe SectionRole), symbolOffset :: !Word64 } deriving (Eq, Show);
data Relocation = Relocation { relocationOffset :: !Word64, relocationKind :: !FixupKind, relocationSymbol :: !Int, relocationAddend :: !Int64 } deriving (Eq, Show);
data ImageSection = ImageSection { imageSectionRole :: !SectionRole, imageSectionAlignment :: !Int, imageSectionSize :: !Word64, imageSectionBytes :: !BL.ByteString, imageSectionRelocations :: ![Relocation] } deriving (Eq, Show);
data Image = Image { imageSections :: ![ImageSection], imageSymbols :: ![Symbol] } deriving (Eq, Show);
imageMetadata :: Image -> Image;
imageMetadata image = image {imageSections = map (\section -> section {imageSectionBytes = BL.empty}) (imageSections image)};
data ObjectError = ObjectNoSection | ObjectDuplicateSymbol !Text | ObjectMissingSymbol !Text | ObjectInvalidAlignment !Int | ObjectDisplacementOutOfRange !Text | ObjectInvalidFixup !FixupKind | ObjectInvalidInput !Text | ObjectSizeOverflow !Text deriving (Eq, Show);
ok :: Either ObjectError ();
ok = Right ();
applyAll :: (value -> ST s (Either err ())) -> [value] -> ST s (Either err ());
applyAll action = go where {
  go values = case values of {
  [] -> pure (Right ());
  value : rest -> do {
  result <- action value;
  case result of {
  Left err -> pure (Left err);
  Right () -> go rest}}}};
data Column v s value = Column !value !(MutVar s (v s value));
newColumn :: (MG.MVector v value) => value -> Int -> ST s (Column v s value);
newColumn fallback capacity = Column fallback <$> (MG.replicate capacity fallback >>= newMutVar);
readColumn :: (MG.MVector v value) => Column v s value -> Int -> ST s value;
readColumn (Column fallback ref) index = do {
  vector <- readMutVar ref;
  if index < MG.length vector then MG.unsafeRead vector index else pure fallback};
writeColumn :: (MG.MVector v value) => Column v s value -> Int -> value -> ST s ();
writeColumn (Column fallback ref) index value = do {
  vector <- readMutVar ref;
  let {
  size = MG.length vector};
  grown <- if index < size then pure vector else do {
  let {
  size' = max (index + 1) (2 * size)};
  larger <- MG.unsafeGrow vector (size' - size);
  MG.set (MG.unsafeSlice size (size' - size) larger) fallback;
  writeMutVar ref larger;
  pure larger};
  MG.unsafeWrite grown index value};
type Unboxed = Column MU.MVector;
type Boxed = Column MV.MVector;
undefinedSection :: Int8;
undefinedSection = -1;
chunkBytes :: Int;
chunkBytes = 65536;
chunkShift :: Int;
chunkShift = 16;
chunkMask :: Int;
chunkMask = chunkBytes - 1;
data Section s = Section { sectionRole :: !SectionRole, sectionCounters :: !(MutablePrimArray s Int), sectionChunks :: !(Boxed s (MutableByteArray s)), fixupOffsets :: !(Unboxed s Int), fixupAddends :: !(Unboxed s Int64), fixupTargets :: !(Unboxed s Int), fixupKinds :: !(Unboxed s Word8) };
counterSize, counterAlignment, counterChunks, counterFixups, counterSealed :: Int;
counterSize = 0;
counterAlignment = 1;
counterChunks = 2;
counterFixups = 3;
counterSealed = 4;
newSection :: SectionRole -> ST s (Section s);
newSection role = do {
  counters <- newPrimArray 5;
  setPrimArray counters 0 5 0;
  first <- newPinnedByteArray chunkBytes;
  Section role counters <$> newColumn first 4 <*> newColumn 0 64 <*> newColumn 0 64 <*> newColumn 0 64 <*> newColumn 0 64};
sectionSize :: Section s -> ST s Int;
sectionSize section = readPrimArray (sectionCounters section) counterSize;
chunkAt :: Section s -> Int -> ST s (MutableByteArray s);
chunkAt section index = do {
  count <- readPrimArray (sectionCounters section) counterChunks;
  if index < count then readColumn (sectionChunks section) index else do {
  chunk <- newPinnedByteArray chunkBytes;
  writeColumn (sectionChunks section) count chunk;
  writePrimArray (sectionCounters section) counterChunks (count + 1);
  pure chunk}};
pokeWord :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ();
pokeWord chunk offset width value = go 0 where {
  go index = when (index < width) $ do {
  writeByteArray chunk (offset + index) (fromIntegral (value `shiftR` (8 * index)) :: Word8);
  go (index + 1)}};
readByte :: MutableByteArray s -> Int -> ST s Word8;
readByte = readByteArray;
peekWord :: MutableByteArray s -> Int -> Int -> ST s Word64;
peekWord chunk offset width = go 0 0 where {
  go index !value | index < width = do {
  byte <- readByte chunk (offset + index);
  go (index + 1) (value .|. (fromIntegral byte `shiftL` (8 * index)))} | otherwise = pure value};
emitByte :: Section s -> Word8 -> ST s ();
emitByte section value = do {
  size <- sectionSize section;
  chunk <- chunkAt section (size `shiftR` chunkShift);
  writeByteArray chunk (size .&. chunkMask) value;
  writePrimArray (sectionCounters section) counterSize (size + 1)};
emitWord :: Section s -> Int -> Word64 -> ST s ();
emitWord section width value = do {
  size <- sectionSize section;
  let {
  within = size .&. chunkMask};
  if within + width <= chunkBytes then do {
  chunk <- chunkAt section (size `shiftR` chunkShift);
  pokeWord chunk within width value;
  writePrimArray (sectionCounters section) counterSize (size + width)} else mapM_ (\index -> emitByte section (fromIntegral (value `shiftR` (8 * index)))) [0 .. width - 1]};
emitBytes :: Section s -> ByteString -> ST s ();
emitBytes section value = go 0 where {
  source = case toShort value of {
  SBS array -> ByteArray array};
  total = BS.length value;
  go done = when (done < total) $ do {
  size <- sectionSize section;
  let {
  within = size .&. chunkMask;
  count = min (total - done) (chunkBytes - within)};
  chunk <- chunkAt section (size `shiftR` chunkShift);
  copyByteArray chunk within source done count;
  writePrimArray (sectionCounters section) counterSize (size + count);
  go (done + count)}};
readWordAt :: Section s -> Int -> Int -> ST s Word64;
readWordAt section offset width = do {
  let {
  within = offset .&. chunkMask};
  if within + width <= chunkBytes then do {
  chunk <- chunkAt section (offset `shiftR` chunkShift);
  peekWord chunk within width} else do {
  bytes <- mapM (\index -> do {
  chunk <- chunkAt section ((offset + index) `shiftR` chunkShift);
  readByte chunk ((offset + index) .&. chunkMask)}) [0 .. width - 1];
  pure (foldr (\byte value -> (value `shiftL` 8) .|. fromIntegral byte) 0 bytes)}};
writeWordAt :: Section s -> Int -> Int -> Word64 -> ST s ();
writeWordAt section offset width value = do {
  let {
  within = offset .&. chunkMask};
  if within + width <= chunkBytes then do {
  chunk <- chunkAt section (offset `shiftR` chunkShift);
  pokeWord chunk within width value} else mapM_ (\index -> do {
  chunk <- chunkAt section ((offset + index) `shiftR` chunkShift);
  writeByteArray chunk ((offset + index) .&. chunkMask) (fromIntegral (value `shiftR` (8 * index)) :: Word8)}) [0 .. width - 1]};
freezeSection :: Section s -> ST s BL.ByteString;
freezeSection section = do {
  size <- sectionSize section;
  count <- readPrimArray (sectionCounters section) counterChunks;
  pieces <- mapM (\index -> do {
  ByteArray frozen <- readColumn (sectionChunks section) index >>= unsafeFreezeByteArray;
  let {
  width = if index == count - 1 then size - index * chunkBytes else chunkBytes};
  pure (BS.take width (fromShort (SBS frozen)))}) [0 .. count - 1];
  pure (BL.fromChunks pieces)};
data FixupRow = FixupRow { fixupRowOffset :: !Int, fixupRowAddend :: !Int64, fixupRowTarget :: !Int, fixupRowKind :: !FixupKind };
readFixup :: Section s -> Int -> ST s FixupRow;
readFixup section row = FixupRow <$> readColumn (fixupOffsets section) row <*> readColumn (fixupAddends section) row <*> readColumn (fixupTargets section) row <*> (toEnum . fromIntegral <$> readColumn (fixupKinds section) row);
writeFixup :: Section s -> Int -> FixupRow -> ST s ();
writeFixup section row fixup = do {
  writeColumn (fixupOffsets section) row (fixupRowOffset fixup);
  writeColumn (fixupAddends section) row (fixupRowAddend fixup);
  writeColumn (fixupTargets section) row (fixupRowTarget fixup);
  writeColumn (fixupKinds section) row (fromIntegral (fromEnum (fixupRowKind fixup)))};
data Object s = Object { objectCurrent :: !(MutVar s (Maybe (Section s))), objectSections :: !(MutVar s (Map SectionRole (Section s))), objectOrder :: !(MutVar s [SectionRole]), objectSymbolIds :: !(MutVar s (Map Text Int)), objectSymbolNames :: !(MutVar s [Text]), objectSymbolCount :: !(MutablePrimArray s Int), objectSymbolOffsets :: !(Unboxed s Int), objectSymbolSections :: !(Unboxed s Int8), objectSymbolGlobals :: !(Unboxed s Bool), objectLocalOffsets :: !(Unboxed s Int), objectLocalSections :: !(Unboxed s Int8) };
newObject :: ST s (Object s);
newObject = do {
  count <- newPrimArray 1;
  writePrimArray count 0 0;
  Object <$> newMutVar Nothing <*> newMutVar Map.empty <*> newMutVar [] <*> newMutVar Map.empty <*> newMutVar [] <*> pure count <*> newColumn 0 256 <*> newColumn undefinedSection 256 <*> newColumn False 256 <*> newColumn 0 256 <*> newColumn undefinedSection 256};
assembleObject :: (ObjectError -> err) -> (Image -> Either ObjectError BL.ByteString) -> (forall s. Object s -> ST s (Either err ())) -> Either err BL.ByteString;
assembleObject wrap encode build = runST $ do {
  object <- newObject;
  result <- build object;
  case result of {
  Left err -> pure (Left err);
  Right () -> either (Left . wrap) (either (Left . wrap) Right . encode) <$> layoutObject object}};
currentSectionRole :: Object s -> ST s (Maybe SectionRole);
currentSectionRole object = fmap sectionRole <$> readMutVar (objectCurrent object);
selectSection :: SectionRole -> Object s -> ST s ();
selectSection role object = do {
  current <- readMutVar (objectCurrent object);
  case current of {
  Just section | sectionRole section == role -> pure ();
  _ -> do {
  sections <- readMutVar (objectSections object);
  section <- case Map.lookup role sections of {
  Just section -> pure section;
  Nothing -> do {
  section <- newSection role;
  writeMutVar (objectSections object) (Map.insert role section sections);
  modifyMutVar' (objectOrder object) (role :);
  pure section}};
  writeMutVar (objectCurrent object) (Just section)}}};
internSymbol :: Object s -> Text -> ST s Int;
internSymbol object name = do {
  ids <- readMutVar (objectSymbolIds object);
  case Map.lookup name ids of {
  Just identifier -> pure identifier;
  Nothing -> do {
  identifier <- readPrimArray (objectSymbolCount object) 0;
  writePrimArray (objectSymbolCount object) 0 (identifier + 1);
  writeColumn (objectSymbolSections object) identifier undefinedSection;
  writeMutVar (objectSymbolIds object) (Map.insert name identifier ids);
  modifyMutVar' (objectSymbolNames object) (name :);
  pure identifier}}};
addGlobal :: Text -> Object s -> ST s ();
addGlobal name object = do {
  identifier <- internSymbol object name;
  writeColumn (objectSymbolGlobals object) identifier True};
localText :: Int -> Text;
localText identifier = ".L" <> T.pack (show identifier);
defineLabel :: Object s -> Section s -> Name -> ST s (Either ObjectError ());
defineLabel object section name = do {
  size <- sectionSize section;
  let {
  role = fromIntegral (fromEnum (sectionRole section)) :: Int8};
  case name of {
  SymbolName text -> do {
  identifier <- internSymbol object text;
  defined <- readColumn (objectSymbolSections object) identifier;
  if defined /= undefinedSection then pure (Left (ObjectDuplicateSymbol text)) else do {
  writeColumn (objectSymbolOffsets object) identifier size;
  writeColumn (objectSymbolSections object) identifier role;
  pure ok}};
  LocalName identifier _ -> do {
  defined <- readColumn (objectLocalSections object) identifier;
  if defined /= undefinedSection then pure (Left (ObjectDuplicateSymbol (localText identifier))) else do {
  writeColumn (objectLocalOffsets object) identifier size;
  writeColumn (objectLocalSections object) identifier role;
  pure ok}}}};
emitItem :: Object s -> Item -> ST s (Either ObjectError ());
emitItem object item = do {
  current <- readMutVar (objectCurrent object);
  case current of {
  Nothing -> pure (Left ObjectNoSection);
  Just section -> case item of {
  Word width value -> emitWord section width value >> pure ok;
  Bytes value -> emitBytes section value >> pure ok;
  Label name -> defineLabel object section name;
  Apply fixup -> do {
  size <- sectionSize section;
  target <- case fixupTarget fixup of {
  SymbolName text -> internSymbol object text;
  LocalName identifier _ -> pure (-1 - identifier)};
  emitWord section (fixupWidth fixup) (fixupWord fixup);
  row <- readPrimArray (sectionCounters section) counterFixups;
  writePrimArray (sectionCounters section) counterFixups (row + 1);
  writeFixup section row (FixupRow size (fixupAddend fixup) target (fixupKind fixup));
  pure ok}}}};
emitItems :: Object s -> [Item] -> ST s (Either ObjectError ());
emitItems object = applyAll (emitItem object);
emitAlign :: Object s -> Int -> ByteString -> ST s (Either ObjectError ());
emitAlign object alignmentPower fill = do {
  current <- readMutVar (objectCurrent object);
  case current of {
  Nothing -> pure (Left ObjectNoSection);
  Just section | alignmentPower < 0 || alignmentPower > 30 -> pure (Left (ObjectInvalidAlignment alignmentPower)) | BS.null fill -> pure (Left (ObjectInvalidInput "empty alignment fill")) | otherwise -> do {
  size <- sectionSize section;
  alignment <- readPrimArray (sectionCounters section) counterAlignment;
  writePrimArray (sectionCounters section) counterAlignment (max alignment alignmentPower);
  let {
  boundary = 1 `shiftL` alignmentPower;
  padding = (boundary - size `mod` boundary) `mod` boundary;
  block = BS.concat (replicate (max 1 (min padding 4096 `div` BS.length fill)) fill);
  go remaining = when (remaining > 0) $ do {
  let {
  count = min remaining (BS.length block)};
  emitBytes section (BS.take count block);
  go (remaining - count)}};
  go padding;
  pure ok}}};
sealFunction :: Object s -> ST s (Either ObjectError ());
sealFunction object = do {
  current <- readMutVar (objectCurrent object);
  case current of {
  Nothing -> pure ok;
  Just section -> do {
  sealed <- readPrimArray (sectionCounters section) counterSealed;
  count <- readPrimArray (sectionCounters section) counterFixups;
  let {
  go source target | source == count = do {
  writePrimArray (sectionCounters section) counterFixups target;
  writePrimArray (sectionCounters section) counterSealed target;
  pure ok} | otherwise = do {
  fixup <- readFixup section source;
  if fixupRowTarget fixup < 0 then do {
  result <- resolveLocal object section fixup;
  case result of {
  Left err -> pure (Left err);
  Right () -> go (source + 1) target}} else do {
  when (source /= target) (writeFixup section target fixup);
  go (source + 1) (target + 1)}}};
  go sealed sealed}}};
resolveLocal :: Object s -> Section s -> FixupRow -> ST s (Either ObjectError ());
resolveLocal object section fixup = do {
  let {
  identifier = -1 - fixupRowTarget fixup};
  defined <- readColumn (objectLocalSections object) identifier;
  if defined == undefinedSection then pure (Left (ObjectMissingSymbol (localText identifier))) else if fromIntegral defined /= fromEnum (sectionRole section) || not (canResolve (fixupRowKind fixup)) then pure (Left (ObjectInvalidFixup (fixupRowKind fixup))) else do {
  target <- readColumn (objectLocalOffsets object) identifier;
  patchLocal section (localText identifier) target fixup}};
canResolve :: FixupKind -> Bool;
canResolve kind = case kind of {
  Arm64Branch26 -> True;
  Arm64Branch19 -> True;
  Arm64Adr21 -> True;
  X86Pc32 -> True;
  X86Plt32 -> True;
  _ -> False};
patchLocal :: Section s -> Text -> Int -> FixupRow -> ST s (Either ObjectError ());
patchLocal section name target fixup = do {
  word <- readWordAt section offset 4;
  let {
  instruction = fromIntegral word :: Word32};
  case fixupRowKind fixup of {
  Arm64Branch26 | displacement `mod` 4 /= 0 || not (fitsSigned 28 displacement) -> outOfRange | otherwise -> write (instruction .|. fromIntegral ((displacement `shiftR` 2) .&. 0x03ffffff));
  Arm64Branch19 | displacement `mod` 4 /= 0 || not (fitsSigned 21 displacement) -> outOfRange | otherwise -> write (instruction .|. fromIntegral (((displacement `shiftR` 2) .&. 0x7ffff) `shiftL` 5));
  Arm64Adr21 | not (fitsSigned 21 displacement) -> outOfRange | otherwise -> let {
  immediate = displacement .&. 0x1fffff;
  low = fromIntegral ((immediate .&. 3) `shiftL` 29);
  high = fromIntegral (((immediate `shiftR` 2) .&. 0x7ffff) `shiftL` 5)} in write (instruction .|. low .|. high);
  X86Pc32 -> patchX86;
  X86Plt32 -> patchX86;
  kind -> pure (Left (ObjectInvalidFixup kind))}} where {
  offset = fixupRowOffset fixup;
  displacement = fromIntegral target - fromIntegral offset + fixupRowAddend fixup :: Int64;
  outOfRange = pure (Left (ObjectDisplacementOutOfRange name));
  write value = writeWordAt section offset 4 (fromIntegral (value :: Word32)) >> pure ok;
  patchX86 | fitsSigned 32 displacement = write (fromIntegral displacement) | otherwise = outOfRange};
fitsSigned :: Int -> Int64 -> Bool;
fitsSigned bits value = value >= negate (1 `shiftL` (bits - 1)) && value < (1 `shiftL` (bits - 1));
layoutObject :: Object s -> ST s (Either ObjectError Image);
layoutObject object = do {
  order <- reverse <$> readMutVar (objectOrder object);
  sections <- readMutVar (objectSections object);
  symbolCount <- readPrimArray (objectSymbolCount object) 0;
  names <- listArray (0, symbolCount - 1) . reverse <$> readMutVar (objectSymbolNames object);
  relocated <- MU.replicate symbolCount False;
  resolved <- resolveSections object (names !) relocated [sections Map.! role | role <- order];
  case resolved of {
  Left err -> pure (Left err);
  Right sectionRelocations -> do {
  ids <- Map.toAscList <$> readMutVar (objectSymbolIds object);
  candidates <- mapM (\named@(_, identifier) -> (,) named <$> readSymbol identifier) ids;
  needed <- filterM (\((_, identifier), row) -> if symbolRowGlobal row then pure True else MU.unsafeRead relocated identifier) candidates;
  let {
  defined row = symbolRowSection row /= undefinedSection;
  privateLabels = IntMap.fromList (zip [identifier | ((_, identifier), row) <- needed, not (symbolRowGlobal row), defined row] [".L" <> T.pack (show index) | index <- [0 :: Int ..]]);
  emitted (name, identifier) = IntMap.findWithDefault name identifier privateLabels;
  ordered = sortOn fst [(emitted named, (named, row)) | (named, row) <- needed];
  symbols = [if defined row then Symbol label (symbolRowGlobal row) (Just (toEnum (fromIntegral (symbolRowSection row)))) (fromIntegral (symbolRowOffset row)) else Symbol label True Nothing 0 | (label, (_, row)) <- ordered]};
  positions <- MU.replicate symbolCount (-1 :: Int);
  mapM_ (\(position, (_, ((_, identifier), _))) -> MU.unsafeWrite positions identifier position) (zip [0 ..] ordered);
  imageSections <- mapM (\(section, relocations) -> do {
  size <- sectionSize section;
  alignment <- readPrimArray (sectionCounters section) counterAlignment;
  bytes <- freezeSection section;
  placed <- mapM (\(offset, kind, identifier, addend) -> (\position -> Relocation offset kind position addend) <$> MU.unsafeRead positions identifier) relocations;
  pure ImageSection {imageSectionRole = sectionRole section, imageSectionAlignment = alignment, imageSectionSize = fromIntegral size, imageSectionBytes = bytes, imageSectionRelocations = placed}}) sectionRelocations;
  pure (Right Image {imageSections = imageSections, imageSymbols = symbols})}}} where {
  readSymbol identifier = SymbolRow <$> readColumn (objectSymbolOffsets object) identifier <*> readColumn (objectSymbolSections object) identifier <*> readColumn (objectSymbolGlobals object) identifier};
data SymbolRow = SymbolRow { symbolRowOffset :: !Int, symbolRowSection :: !Int8, symbolRowGlobal :: !Bool };
resolveSections :: Object s -> (Int -> Text) -> MU.MVector s Bool -> [Section s] -> ST s (Either ObjectError [(Section s, [(Word64, FixupKind, Int, Int64)])]);
resolveSections object nameOf relocated = go [] where {
  go done sections = case sections of {
  [] -> pure (Right (reverse done));
  section : rest -> do {
  count <- readPrimArray (sectionCounters section) counterFixups;
  result <- resolveFrom section 0 count [];
  case result of {
  Left err -> pure (Left err);
  Right relocations -> go ((section, relocations) : done) rest}}};
  resolveFrom section index count relocations | index == count = pure (Right (reverse relocations)) | otherwise = do {
  fixup <- readFixup section index;
  result <- if fixupRowTarget fixup < 0 then fmap (const Nothing) <$> resolveLocal object section fixup else do {
  let {
  identifier = fixupRowTarget fixup};
  definedIn <- readColumn (objectSymbolSections object) identifier;
  global <- readColumn (objectSymbolGlobals object) identifier;
  let {
  sameSection = fromIntegral definedIn == fromEnum (sectionRole section)};
  if canResolve (fixupRowKind fixup) && not global && sameSection then do {
  target <- readColumn (objectSymbolOffsets object) identifier;
  fmap (const Nothing) <$> patchLocal section (nameOf identifier) target fixup} else do {
  MU.unsafeWrite relocated identifier True;
  pure (Right (Just (fromIntegral (fixupRowOffset fixup), fixupRowKind fixup, identifier, fixupRowAddend fixup)))}};
  case result of {
  Left err -> pure (Left err);
  Right Nothing -> resolveFrom section (index + 1) count relocations;
  Right (Just relocation) -> resolveFrom section (index + 1) count (relocation : relocations)}}}
}
