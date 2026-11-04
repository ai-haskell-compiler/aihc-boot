{-# LANGUAGE RankNTypes #-}

-- | Generic programming over data types: the class 'Data', after the
-- module of GHC's base.
--
-- The boot base has no Rational yet, so there is no float
-- representation ('FloatRep', 'FloatConstr', 'mkFloatType').
module Data.Data
  ( module Data.Typeable,
    Data (..),
    DataType,
    Constr,
    ConIndex,
    Fixity (..),
    DataRep (..),
    ConstrRep (..),
    mkDataType,
    mkConstr,
    mkConstrTag,
    mkIntType,
    mkCharType,
    mkNoRepType,
    mkIntegralConstr,
    mkCharConstr,
    dataTypeName,
    dataTypeRep,
    dataTypeConstrs,
    constrType,
    constrRep,
    constrFields,
    constrFixity,
    constrIndex,
    showConstr,
    readConstr,
    indexConstr,
    maxConstrIndex,
    isAlgType,
    fromConstr,
    fromConstrB,
    tyconUQname,
    tyconModule,
  )
where

import Data.Typeable
import GHC.Base
import GHC.Classes
import GHC.Err
import GHC.List
import GHC.Maybe
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Real
import GHC.Show
import GHC.Types

-- | The representation of a data type.
data DataType = DataType
  { tycon :: String,
    datarep :: DataRep
  }

instance Show DataType where
  showsPrec d (DataType t r) =
    showParen (d >= 11) (showString "DataType {tycon = " . showsPrec 0 t . showString ", datarep = " . showsPrec 0 r . showString "}")

-- | The representation of a constructor.
data Constr = Constr
  { conrep :: ConstrRep,
    constring :: String,
    confields :: [String],
    confixity :: Fixity,
    datatype :: DataType
  }

instance Show Constr where
  show = constring

instance Eq Constr where
  c == c' = constrRep c == constrRep c'

data DataRep
  = AlgRep [Constr]
  | IntRep
  | CharRep
  | NoRep
  deriving (Eq, Show)

data ConstrRep
  = AlgConstr ConIndex
  | IntConstr Integer
  | CharConstr Char
  deriving (Eq, Show)

-- | The index of a constructor, from 1.
type ConIndex = Int

data Fixity
  = Prefix
  | Infix
  deriving (Eq, Show)

class (Typeable a) => Data a where
  gfoldl :: (forall d b. (Data d) => c (d -> b) -> d -> c b) -> (forall g. g -> c g) -> a -> c a
  gunfold :: (forall b r. (Data b) => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c a
  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType
  dataCast1 :: (Typeable t) => (forall d. (Data d) => c (t d)) -> Maybe (c a)
  dataCast2 :: (Typeable t) => (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c a)
  gmapT :: (forall b. (Data b) => b -> b) -> a -> a
  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. (Data d) => d -> r') -> a -> r
  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. (Data d) => d -> r') -> a -> r
  gmapQ :: (forall d. (Data d) => d -> u) -> a -> [u]
  gmapQi :: forall u. Int -> (forall d. (Data d) => d -> u) -> a -> u
  gmapM :: forall m. (Monad m) => (forall d. (Data d) => d -> m d) -> a -> m a
  gfoldl _ z = z
  dataCast1 _ = Nothing
  dataCast2 _ = Nothing
  gmapT f x0 = unID (gfoldl (\(ID c) x -> ID (c (f x))) ID x0)
  gmapQl o r f x0 = unCONST (gfoldl (\c x -> CONST (unCONST c `o` f x)) (\_ -> CONST r) x0)
  gmapQr o r0 f x0 = unQr (gfoldl (\(Qr c) x -> Qr (\r -> c (f x `o` r))) (\_ -> Qr id) x0) r0
  gmapQ f = gmapQr (:) [] f
  gmapQi i f x = case gfoldl (\(Qi n q) a -> Qi (n + 1) (if i == n then Just (f a) else q)) (\_ -> Qi 0 Nothing) x of
    Qi _ q -> case q of
      Just u -> u
      Nothing -> errorWithoutStackTrace "Data.Data.gmapQi: index out of range"
  gmapM f = gfoldl (\c x -> c >>= \c' -> f x >>= \x' -> return (c' x')) return

newtype ID x = ID x

unID :: ID x -> x
unID (ID x) = x

newtype CONST c a = CONST c

unCONST :: CONST c a -> c
unCONST (CONST c) = c

newtype Qr r a = Qr (r -> r)

unQr :: Qr r a -> r -> r
unQr (Qr f) = f

data Qi q a = Qi Int (Maybe q)

-- | Build a term from a constructor whose fields are all undefined.
fromConstr :: (Data a) => Constr -> a
fromConstr = fromConstrB (errorWithoutStackTrace "Data.Data.fromConstr")

-- | Build a term from a constructor and a generic field value.
fromConstrB :: (Data a) => (forall d. (Data d) => d) -> Constr -> a
fromConstrB f = unID . gunfold (\(ID c) -> ID (c f)) ID

dataTypeName :: DataType -> String
dataTypeName = tycon

dataTypeRep :: DataType -> DataRep
dataTypeRep = datarep

constrType :: Constr -> DataType
constrType = datatype

constrRep :: Constr -> ConstrRep
constrRep = conrep

repConstr :: DataType -> ConstrRep -> Constr
repConstr dt cr = case (dataTypeRep dt, cr) of
  (AlgRep cs, AlgConstr i) -> cs !! (i - 1)
  (IntRep, IntConstr i) -> mkIntegralConstr dt i
  (CharRep, CharConstr c) -> mkCharConstr dt c
  _ -> errorWithoutStackTrace "Data.Data.repConstr: The given ConstrRep does not fit to the given DataType."

mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType str (AlgRep cs)

mkConstrTag :: DataType -> String -> Int -> [String] -> Fixity -> Constr
mkConstrTag dt str idx fields fix = Constr (AlgConstr idx) str fields fix dt

mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix = mkConstrTag dt str idx fields fix
  where
    idx = case [i | (c, i) <- zip (dataTypeConstrs dt) [1 ..], showConstr c == str] of
      (i : _) -> i
      [] -> errorWithoutStackTrace ("Data.Data.mkConstr: couldn't find constructor " ++ str)

dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
  AlgRep cons -> cons
  _ -> errorWithoutStackTrace ("Data.Data.dataTypeConstrs is not supported for " ++ dataTypeName dt ++ ", as it is not an algebraic data type.")

constrFields :: Constr -> [String]
constrFields = confields

constrFixity :: Constr -> Fixity
constrFixity = confixity

showConstr :: Constr -> String
showConstr = constring

-- | The constructor of a data type with a given name.
readConstr :: DataType -> String -> Maybe Constr
readConstr dt str = case dataTypeRep dt of
  AlgRep cons -> case filter (\c -> showConstr c == str) cons of
    (c : _) -> Just c
    [] -> Nothing
  _ -> Nothing

isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
  AlgRep _ -> True
  _ -> False

indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
  AlgRep cs -> cs !! (idx - 1)
  _ -> errorWithoutStackTrace ("Data.Data.indexConstr is not supported for " ++ dataTypeName dt ++ ", as it is not an algebraic data type.")

constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
  AlgConstr idx -> idx
  _ -> errorWithoutStackTrace ("Data.Data.constrIndex is not supported for " ++ dataTypeName (constrType con) ++ ", as it is not an algebraic data type.")

maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
  AlgRep cs -> length cs
  _ -> errorWithoutStackTrace ("Data.Data.maxConstrIndex is not supported for " ++ dataTypeName dt ++ ", as it is not an algebraic data type.")

mkIntType :: String -> DataType
mkIntType str = DataType str IntRep

mkCharType :: String -> DataType
mkCharType str = DataType str CharRep

mkNoRepType :: String -> DataType
mkNoRepType str = DataType str NoRep

mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkIntegralConstr dt i = case datarep dt of
  IntRep -> mkPrimCon dt (show i) (IntConstr (toInteger i))
  _ -> errorWithoutStackTrace ("Data.Data.mkIntegralConstr is not supported for " ++ dataTypeName dt ++ ", as it is not an Integral data type.")

mkCharConstr :: DataType -> Char -> Constr
mkCharConstr dt c = case datarep dt of
  CharRep -> mkPrimCon dt (show c) (CharConstr c)
  _ -> errorWithoutStackTrace ("Data.Data.mkCharConstr is not supported for " ++ dataTypeName dt ++ ", as it is not an Char data type.")

mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dt str cr =
  Constr cr str (errorWithoutStackTrace "Data.Data.confields") (errorWithoutStackTrace "Data.Data.confixity") dt

-- | The name of a type constructor without its module.
tyconUQname :: String -> String
tyconUQname x = case dropWhile (/= '.') x of
  [] -> x
  (_ : tl) -> tyconUQname tl

-- | The module of a type constructor.
tyconModule :: String -> String
tyconModule x = case break (== '.') x of
  (_, "") -> ""
  (a, _ : tl) -> case tyconModule tl of
    "" -> a
    m -> a ++ "." ++ m

-- Instances for the basic types.

intType :: DataType
intType = mkIntType "Prelude.Int"

instance Data Int where
  toConstr x = mkIntegralConstr intType x
  gunfold _ z c = case constrRep c of
    IntConstr x -> z (fromInteger x)
    _ -> errorWithoutStackTrace "Data.Data.gunfold: Constructor is not of type Int."
  dataTypeOf _ = intType

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

instance Data Integer where
  toConstr = mkIntegralConstr integerType
  gunfold _ z c = case constrRep c of
    IntConstr x -> z x
    _ -> errorWithoutStackTrace "Data.Data.gunfold: Constructor is not of type Integer."
  dataTypeOf _ = integerType

charType :: DataType
charType = mkCharType "Prelude.Char"

instance Data Char where
  toConstr x = mkCharConstr charType x
  gunfold _ z c = case constrRep c of
    CharConstr x -> z x
    _ -> errorWithoutStackTrace "Data.Data.gunfold: Constructor is not of type Char."
  dataTypeOf _ = charType

falseConstr :: Constr
falseConstr = mkConstrTag boolDataType "False" 1 [] Prefix

trueConstr :: Constr
trueConstr = mkConstrTag boolDataType "True" 2 [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Bool" [falseConstr, trueConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True = trueConstr
  gunfold _ z c = case constrIndex c of
    1 -> z False
    _ -> z True
  dataTypeOf _ = boolDataType

unitConstr :: Constr
unitConstr = mkConstrTag unitDataType "()" 1 [] Prefix

unitDataType :: DataType
unitDataType = mkDataType "Unit" [unitConstr]

instance Data () where
  toConstr () = unitConstr
  gunfold _ z _ = z ()
  dataTypeOf _ = unitDataType

nilConstr :: Constr
nilConstr = mkConstrTag listDataType "[]" 1 [] Prefix

consConstr :: Constr
consConstr = mkConstrTag listDataType "(:)" 2 [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr, consConstr]

instance (Data a) => Data [a] where
  gfoldl _ z [] = z []
  gfoldl f z (x : xs) = z (:) `f` x `f` xs
  toConstr [] = nilConstr
  toConstr (_ : _) = consConstr
  gunfold k z c = case constrIndex c of
    1 -> z []
    _ -> k (k (z (:)))
  dataTypeOf _ = listDataType
  dataCast1 f = gcast1 f
  gmapT _ [] = []
  gmapT f (x : xs) = f x : f xs
  gmapQ _ [] = []
  gmapQ f (x : xs) = [f x, f xs]
  gmapM _ [] = return []
  gmapM f (x : xs) = f x >>= \x' -> f xs >>= \xs' -> return (x' : xs')

nothingConstr :: Constr
nothingConstr = mkConstrTag maybeDataType "Nothing" 1 [] Prefix

justConstr :: Constr
justConstr = mkConstrTag maybeDataType "Just" 2 [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr, justConstr]

instance (Data a) => Data (Maybe a) where
  gfoldl _ z Nothing = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
    1 -> z Nothing
    _ -> k (z Just)
  dataTypeOf _ = maybeDataType
  dataCast1 f = gcast1 f

tuple2Constr :: Constr
tuple2Constr = mkConstrTag tuple2DataType "(,)" 1 [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

instance (Data a, Data b) => Data (a, b) where
  gfoldl f z (a, b) = z (,) `f` a `f` b
  toConstr (_, _) = tuple2Constr
  gunfold k z _ = k (k (z (,)))
  dataTypeOf _ = tuple2DataType
