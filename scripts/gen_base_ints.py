#!/usr/bin/env python3
"""Write vendor/base/GHC/Word.hs, GHC/Int.hs and Foreign/C/Types.hs.

The fixed-size integer types of the boot base have the same instances,
with small differences. This script writes the instances from one
template. Run it from the repository root after a change of the
template, and commit the result:

    python3 scripts/gen_base_ints.py

A value of a fixed-size type is an Int at run time. An unsigned type of
n < 64 bits keeps the value in 0 .. 2^n - 1, a signed type keeps it sign
extended, and a 64-bit type keeps all the bits. Each operation works on
the Int and then narrows the result.
"""

import os

HEADER = """-- | {doc}
--
-- This file comes from scripts/gen_base_ints.py. Do not edit it.
module {module}
  ( {exports}
  )
where

import GHC.Base
import GHC.Bits
import GHC.Classes
import GHC.Enum
import GHC.Err
import GHC.Maybe
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Real
import GHC.Show
import GHC.Types
"""

TEMPLATE = """
{decl}

to{t} :: {t} -> Int
to{t} = primUnsafeCoerce

from{t} :: Int -> {t}
from{t} x = primUnsafeCoerce ({narrow})

instance Eq {t} where
  a == b = primIntEq (to{t} a) (to{t} b)
  a /= b = primIntNe (to{t} a) (to{t} b)

instance Ord {t} where
  compare a b = if {lt} (to{t} a) (to{t} b) then LT else if primIntEq (to{t} a) (to{t} b) then EQ else GT
  a < b = {lt} (to{t} a) (to{t} b)
  a <= b = {le} (to{t} a) (to{t} b)
  a > b = {gt} (to{t} a) (to{t} b)
  a >= b = {ge} (to{t} a) (to{t} b)

{showinst}instance Num {t} where
  a + b = from{t} (primIntAdd (to{t} a) (to{t} b))
  a - b = from{t} (primIntSub (to{t} a) (to{t} b))
  a * b = from{t} (primIntMul (to{t} a) (to{t} b))
  negate a = from{t} (primIntNegate (to{t} a))
  {abs}
  signum a = if a == 0 then 0 else {signum}
  fromInteger i = from{t} (integerToInt i)

instance Bounded {t} where
  minBound = {minb}
  maxBound = {maxb}

instance Enum {t} where
  succ x = x + 1
  pred x = x - 1
  toEnum = from{t}
  fromEnum = to{t}
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo x y = if x > y then [] else go x
    where
      go i = i : (if i == y then [] else go (i + 1))
  enumFromThenTo x y z
    | y >= x = if x > z then [] else up x
    | True = if x < z then [] else down x
    where
      step = y - x
      back = x - y
      up i = i : (if i > z - step then [] else up (i + step))
      down i = i : (if i < z + back then [] else down (i - back))

instance Real {t}

instance Integral {t} where
  quot a b = if b == 0 then divZeroError else from{t} ({quot} (to{t} a) (to{t} b))
  rem a b = if b == 0 then divZeroError else from{t} ({rem} (to{t} a) (to{t} b))
  {divmod}
  quotRem a b = (quot a b, rem a b)
  toInteger a = {tointeger}

instance Bits {t} where
  a .&. b = from{t} (primIntAnd (to{t} a) (to{t} b))
  a .|. b = from{t} (primIntOr (to{t} a) (to{t} b))
  xor a b = from{t} (primIntXor (to{t} a) (to{t} b))
  complement a = from{t} (primIntXor (to{t} a) (negate 1))
  zeroBits = 0
  bit i = from{t} (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (to{t} a) i) 1 /= 0
  shiftL a i = from{t} (primIntShl (to{t} a) i)
  unsafeShiftL a i = from{t} (primIntShl (to{t} a) i)
  shiftR a i = from{t} ({shr} (to{t} a) i)
  unsafeShiftR a i = from{t} ({shr} (to{t} a) i)
  rotateL a i =
    let r = i `mod` {n}
        u = {unsigned_bits}
     in if r == 0 then a else from{t} (primIntOr (primIntShl u r) (primIntShrl u ({n} - r)))
  rotateR a i = rotateL a ({n} - (i `mod` {n}))
  bitSizeMaybe _ = Just {n}
  bitSize _ = {n}
  isSigned _ = {signed}
  popCount a = primIntPopCount ({unsigned_bits_a})

instance FiniteBits {t} where
  finiteBitSize _ = {n}
  countLeadingZeros a = primIntClz ({unsigned_bits_a}) - {pad}
  countTrailingZeros a = min {n} (primIntCtz (to{t} a))
"""


def instance(t, n, signed, declare=True):
    mask = (1 << n) - 1
    d = {"t": t, "n": n, "signed": "True" if signed else "False", "pad": 64 - n}
    d["decl"] = "data %s" % t if declare else "-- %s is in GHC.Types." % t
    if n == 64:
        d["narrow"] = "x"
    elif signed:
        d["narrow"] = "primIntShra (primIntShl x %d) %d" % (64 - n, 64 - n)
    else:
        d["narrow"] = "primIntAnd x %d" % mask
    unsigned64 = n == 64 and not signed
    if unsigned64:
        d["lt"], d["le"], d["gt"], d["ge"] = "primWordLt", "primWordLe", "primWordGt", "primWordGe"
        d["show"] = "showsPrec _ a = showString (primWordShow (to%s a))" % t
        d["quot"], d["rem"] = "primWordQuot", "primWordRem"
        d["tointeger"] = (
            "if primIntGe (to%s a) 0 then integerFromInt (to%s a) "
            "else integerAdd (integerFromInt (to%s a)) (integerMul (integerFromInt 4294967296) (integerFromInt 4294967296))" % (t, t, t)
        )
    else:
        d["lt"], d["le"], d["gt"], d["ge"] = "primIntLt", "primIntLe", "primIntGt", "primIntGe"
        d["show"] = "showsPrec d a = showsPrec d (to%s a)" % t
        d["quot"], d["rem"] = "primIntQuot", "primIntRem"
        d["tointeger"] = "integerFromInt (to%s a)" % t
    # Show Word is in GHC.Show, as in GHC, so that a program that does not
    # import GHC.Word has it.
    d["showinst"] = "" if t == "Word" else "instance Show %s where\n  %s\n\n" % (t, d["show"])
    if signed:
        d["abs"] = "abs a = if a < 0 then negate a else a"
        d["signum"] = "if a < 0 then negate 1 else 1"
        d["minb"] = "from%s (primIntShl 1 %d)" % (t, n - 1) if n < 64 else "from%s (primIntShl 1 63)" % t
        d["maxb"] = "from%s (primIntSub (primIntShl 1 %d) 1)" % (t, n - 1) if n < 64 else "from%s (primIntSub (primIntShl 1 63) 1)" % t
        d["shr"] = "primIntShra"
        d["divmod"] = (
            "div a b = if b == 0 then divZeroError else from%s (primIntDiv (to%s a) (to%s b))\n"
            "  mod a b = if b == 0 then divZeroError else from%s (primIntMod (to%s a) (to%s b))\n"
            "  divMod a b = (div a b, mod a b)" % ((t,) * 6)
        )
        if n == 64:
            d["unsigned_bits"] = "to%s a" % t
            d["unsigned_bits_a"] = "to%s a" % t
        else:
            d["unsigned_bits"] = "primIntAnd (to%s a) %d" % (t, mask)
            d["unsigned_bits_a"] = "primIntAnd (to%s a) %d" % (t, mask)
    else:
        d["abs"] = "abs a = a"
        d["signum"] = "1"
        d["minb"] = "0"
        d["maxb"] = "from%s (negate 1)" % t
        d["shr"] = "primIntShrl"
        d["divmod"] = "div = quot\n  mod = rem\n  divMod a b = (quot a b, rem a b)"
        d["unsigned_bits"] = "to%s a" % t
        d["unsigned_bits_a"] = "to%s a" % t
    return TEMPLATE.format(**d)


def write(path, module, doc, types):
    exports = []
    for t, _n, _s, declare in types:
        exports.append(t if declare else t)
        exports.append("to" + t)
        exports.append("from" + t)
    text = HEADER.format(module=module, doc=doc, exports=",\n    ".join(exports))
    for t, n, s, declare in types:
        text += instance(t, n, s, declare)
    with open(path, "w") as f:
        f.write(text)


CTYPES_HEADER = """-- | The C integer types, as newtypes of the fixed-size types.
--
-- This file comes from scripts/gen_base_ints.py. Do not edit it.
module Foreign.C.Types
  ( {exports}
  )
where

import Foreign.Storable
import GHC.Base
import GHC.Bits
import GHC.Classes
import GHC.Enum
import GHC.Int
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Types
import GHC.Word
"""

CTYPE_TEMPLATE = """
newtype {t} = {t} {b}

un{t} :: {t} -> {b}
un{t} ({t} x) = x

instance Eq {t} where
  a == b = un{t} a == un{t} b

instance Ord {t} where
  compare a b = compare (un{t} a) (un{t} b)

instance Show {t} where
  showsPrec d a = showsPrec d (un{t} a)

instance Num {t} where
  a + b = {t} (un{t} a + un{t} b)
  a - b = {t} (un{t} a - un{t} b)
  a * b = {t} (un{t} a * un{t} b)
  negate a = {t} (negate (un{t} a))
  abs a = {t} (abs (un{t} a))
  signum a = {t} (signum (un{t} a))
  fromInteger i = {t} (fromInteger i)

instance Bounded {t} where
  minBound = {t} minBound
  maxBound = {t} maxBound

instance Enum {t} where
  toEnum i = {t} (toEnum i)
  fromEnum a = fromEnum (un{t} a)
  enumFromTo a b = map {t} (enumFromTo (un{t} a) (un{t} b))
  enumFromThenTo a b c = map {t} (enumFromThenTo (un{t} a) (un{t} b) (un{t} c))

instance Real {t}

instance Integral {t} where
  quot a b = {t} (quot (un{t} a) (un{t} b))
  rem a b = {t} (rem (un{t} a) (un{t} b))
  div a b = {t} (div (un{t} a) (un{t} b))
  mod a b = {t} (mod (un{t} a) (un{t} b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (un{t} a)

instance Bits {t} where
  a .&. b = {t} (un{t} a .&. un{t} b)
  a .|. b = {t} (un{t} a .|. un{t} b)
  xor a b = {t} (xor (un{t} a) (un{t} b))
  complement a = {t} (complement (un{t} a))
  zeroBits = {t} zeroBits
  bit i = {t} (bit i)
  testBit a i = testBit (un{t} a) i
  shiftL a i = {t} (shiftL (un{t} a) i)
  shiftR a i = {t} (shiftR (un{t} a) i)
  rotateL a i = {t} (rotateL (un{t} a) i)
  rotateR a i = {t} (rotateR (un{t} a) i)
  bitSizeMaybe a = bitSizeMaybe (un{t} a)
  bitSize a = bitSize (un{t} a)
  isSigned a = isSigned (un{t} a)
  popCount a = popCount (un{t} a)

instance FiniteBits {t} where
  finiteBitSize a = finiteBitSize (un{t} a)

instance Storable {t} where
  sizeOf a = sizeOf (un{t} a)
  alignment a = alignment (un{t} a)
  peekByteOff p off = fmap {t} (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (un{t} a)
"""

CTYPES = [
    ("CChar", "Int8"),
    ("CSChar", "Int8"),
    ("CUChar", "Word8"),
    ("CShort", "Int16"),
    ("CUShort", "Word16"),
    ("CInt", "Int32"),
    ("CUInt", "Word32"),
    ("CLong", "Int64"),
    ("CULong", "Word64"),
    ("CLLong", "Int64"),
    ("CULLong", "Word64"),
    ("CSize", "Word64"),
]


def write_ctypes(path):
    exports = ",\n    ".join("%s (..)" % t for t, _ in CTYPES)
    text = CTYPES_HEADER.format(exports=exports)
    for t, b in CTYPES:
        text += CTYPE_TEMPLATE.format(t=t, b=b)
    with open(path, "w") as f:
        f.write(text)


def main():
    base = os.path.join("vendor", "base", "GHC")
    write(
        os.path.join(base, "Word.hs"),
        "GHC.Word",
        "Unsigned integer types of fixed size.",
        [
            ("Word8", 8, False, True),
            ("Word16", 16, False, True),
            ("Word32", 32, False, True),
            ("Word64", 64, False, True),
            ("Word", 64, False, False),
        ],
    )
    write(
        os.path.join(base, "Int.hs"),
        "GHC.Int",
        "Signed integer types of fixed size.",
        [
            ("Int8", 8, True, True),
            ("Int16", 16, True, True),
            ("Int32", 32, True, True),
            ("Int64", 64, True, True),
        ],
    )
    write_ctypes(os.path.join("vendor", "base", "Foreign", "C", "Types.hs"))


if __name__ == "__main__":
    main()
