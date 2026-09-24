{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Types
  ( Config (..),
    MacroDef (..),
    defaultConfig,
    IncludeKind (..),
    IncludeRequest (..),
    Severity (..),
    Diagnostic (..),
    Result (..),
    Step (..),
    EngineState (..),
    emptyState,
    defineMacro,
    undefMacro,
    setMacroTable,
    macroFirstByte,
    bloomMember,
    CondFrame (..),
    currentActive,
    mkFrame,
    Continuation,
    LineContext (..),
  )
where

import Aihc.Cpp.Cursor (Cursor)
import Control.DeepSeq (NFData)
import Data.Bits (setBit, testBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

-- | Configuration for the C preprocessor.
data Config = Config
  { -- | The name of the input file, used in @#line@ directives and
    -- @__FILE__@ expansion.
    configInputFile :: FilePath,
    -- | User-defined macros. These are expanded as object-like macros.
    -- Note that the values should include any necessary quoting. For
    -- example, to define a string macro, use @"\"value\""@.
    --
    -- Names and bodies are raw bytes: the preprocessor never decodes
    -- them, so a @-D@ flag taken straight from @argv@ can be passed
    -- through unchanged whatever its encoding.
    configMacros :: !(Map ByteString ByteString)
  }

data MacroDef
  = ObjectMacro !ByteString
  | FunctionMacro ![ByteString] !ByteString
  deriving (Eq, Show)

-- | Default configuration with sensible defaults.
--
-- * 'configInputFile' is set to @\"\<input\>\"@
-- * 'configMacros' includes @__DATE__@ and @__TIME__@ set to the Unix epoch
--
-- To customize the date and time macros:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.fromList [("__DATE__", "\"Mar 15 2026\""), ("__TIME__", "\"14:30:00\"")] }
-- >>> configMacros cfg
-- fromList [("__DATE__","\"Mar 15 2026\""),("__TIME__","\"14:30:00\"")]
--
-- To add additional macros while keeping the defaults:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.insert "VERSION" "42" (configMacros defaultConfig) }
-- >>> M.lookup "VERSION" (configMacros cfg)
-- Just "42"
defaultConfig :: Config
defaultConfig =
  Config
    { configInputFile = "<input>",
      configMacros =
        M.fromList
          [ ("__DATE__", "\"Jan  1 1970\""),
            ("__TIME__", "\"00:00:00\"")
          ]
    }

-- | The kind of @#include@ directive.
data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show, Generic, NFData)

-- | Information about a pending @#include@ that needs to be resolved.
data IncludeRequest = IncludeRequest
  { -- | The path specified in the include directive.
    includePath :: !FilePath,
    -- | Whether this is a local (@\"...\"@) or system (@\<...\>@) include.
    includeKind :: !IncludeKind,
    -- | The file that contains the @#include@ directive.
    includeFrom :: !FilePath,
    -- | The line number of the @#include@ directive.
    includeLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | Severity level for diagnostics.
data Severity = Warning | Error deriving (Eq, Show, Generic, NFData)

-- | A diagnostic message emitted during preprocessing.
data Diagnostic = Diagnostic
  { -- | The severity of the diagnostic.
    diagSeverity :: !Severity,
    -- | The diagnostic message text.
    --
    -- Unlike 'Result', a diagnostic is meant to be shown to a human, so
    -- this is 'Text'. Message fragments taken from the source (an
    -- @#error@ message, an include path) are decoded as UTF-8 with
    -- invalid bytes replaced by U+FFFD; that substitution affects the
    -- message only, never 'resultOutput'.
    diagMessage :: !Text,
    -- | The file where the diagnostic occurred.
    diagFile :: !FilePath,
    -- | The line number where the diagnostic occurred.
    diagLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | The result of preprocessing.
data Result = Result
  { -- | The preprocessed output.
    --
    -- Bytes the preprocessor did not itself generate are copied through
    -- verbatim, so the output carries the input's encoding, whatever it
    -- was. See 'Aihc.Cpp.preprocess' for the encoding contract.
    resultOutput :: !ByteString,
    -- | Any diagnostics (warnings or errors) emitted during preprocessing.
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show, Generic, NFData)

-- | A step in the preprocessing process. Either preprocessing is complete
-- ('Done') or an @#include@ directive needs to be resolved ('NeedInclude').
data Step
  = -- | Preprocessing is complete.
    Done !Result
  | -- | An @#include@ directive was encountered. The caller must provide
    -- the contents of the included file (or 'Nothing' if not found),
    -- and preprocessing will continue.
    NeedInclude !IncludeRequest !(Maybe ByteString -> Step)

data EngineState = EngineState
  { stMacros :: !(Map ByteString MacroDef),
    -- | Which bytes any macro name can start with, as a 64-bit set (see
    -- 'macroFirstByte'). Nearly every identifier in a Haskell module names
    -- no macro, and testing one bit rejects it without the string
    -- comparisons a 'Map' lookup would run. Kept in step with 'stMacros'
    -- by 'setMacros'; @_@ is always a member, because @__LINE__@ and
    -- @__FILE__@ are recognised without being in the map.
    stMacroBloom :: {-# UNPACK #-} !Word64,
    -- | The same, restricted to function-like macros, and 0 when a module
    -- defines none — which is the common case, and lets the multi-line
    -- call lookahead skip the line entirely.
    stFunMacroBloom :: {-# UNPACK #-} !Word64,
    stOutput :: !BSB.Builder,
    stOutputLineCount :: {-# UNPACK #-} !Int,
    stDiagnosticsRev :: ![Diagnostic],
    stPragmaOnceFiles :: !(Set FilePath),
    stSkippingDanglingElse :: !Bool,
    stHsBlockCommentDepth :: !Int,
    stCBlockCommentDepth :: !Int,
    stCurrentFile :: !FilePath,
    stCurrentLine :: !Int
  }

emptyState :: FilePath -> EngineState
emptyState filePath =
  EngineState
    { stMacros = M.empty,
      stMacroBloom = underscoreBloom,
      stFunMacroBloom = 0,
      stOutput = mempty,
      stOutputLineCount = 0,
      stDiagnosticsRev = [],
      stPragmaOnceFiles = S.empty,
      stSkippingDanglingElse = False,
      stHsBlockCommentDepth = 0,
      stCBlockCommentDepth = 0,
      stCurrentFile = filePath,
      stCurrentLine = 1
    }

-- | Define a macro, keeping the first-byte blooms in step. Adding a name
-- only ever sets bits, so this costs one @Map@ insert rather than a walk
-- of the whole table.
defineMacro :: ByteString -> MacroDef -> EngineState -> EngineState
defineMacro name def st =
  st
    { stMacros = M.insert name def (stMacros st),
      stMacroBloom = setBit (stMacroBloom st) bit',
      stFunMacroBloom = case def of
        FunctionMacro _ _ -> setBit (stFunMacroBloom st) bit'
        ObjectMacro _ -> stFunMacroBloom st
    }
  where
    bit' = macroFirstByte name

-- | Undefine a macro. Removing a name can clear a bit, which only a full
-- pass can tell, so the blooms are rebuilt; @#undef@ is rare enough for
-- that not to matter.
undefMacro :: ByteString -> EngineState -> EngineState
undefMacro name st = setMacroTable (M.delete name (stMacros st)) st

-- | Replace the macro table wholesale and rebuild the blooms from it.
setMacroTable :: Map ByteString MacroDef -> EngineState -> EngineState
setMacroTable macros st =
  let (allBloom, funBloom) = M.foldrWithKey step (underscoreBloom, 0) macros
   in st
        { stMacros = macros,
          stMacroBloom = allBloom,
          stFunMacroBloom = funBloom
        }
  where
    step name def (allBloom, funBloom) =
      let bit' = macroFirstByte name
          allBloom' = setBit allBloom bit'
       in case def of
            FunctionMacro _ _ -> (allBloom', setBit funBloom bit')
            ObjectMacro _ -> (allBloom', funBloom)

-- | Bloom containing only @_@, the first byte of @__LINE__@ and @__FILE__@.
underscoreBloom :: Word64
underscoreBloom = setBit 0 (macroFirstByte "_")

-- | Which bit of a bloom a name's first byte occupies: the low six bits of
-- that byte. Identifiers start with a letter, @_@, or a byte >= 0x80, and
-- those map to distinct bits across @A-Z@, @a-z@ and @_@, so the filter is
-- exact for ASCII names and merely approximate for the rest.
macroFirstByte :: ByteString -> Int
macroFirstByte name
  | BS.null name = 0
  | otherwise = fromIntegral (BS.head name .&. 0x3F)
{-# INLINE macroFirstByte #-}

-- | Could a name starting with this byte be in the bloom?
bloomMember :: Word64 -> Word8 -> Bool
bloomMember bloom b = testBit bloom (fromIntegral (b .&. 0x3F))
{-# INLINE bloomMember #-}

data CondFrame = CondFrame
  { frameOuterActive :: !Bool,
    frameConditionTrue :: !Bool,
    frameInElse :: !Bool,
    frameCurrentActive :: !Bool
  }

currentActive :: [CondFrame] -> Bool
currentActive [] = True
currentActive (f : _) = frameCurrentActive f

mkFrame :: Bool -> Bool -> CondFrame
mkFrame outer cond =
  CondFrame
    { frameOuterActive = outer,
      frameConditionTrue = cond,
      frameInElse = False,
      frameCurrentActive = outer && cond
    }

type Continuation = EngineState -> Step

data LineContext = LineContext
  { lcFilePath :: !FilePath,
    lcLineNo :: !Int,
    lcLineSpan :: !Int,
    lcNextLineNo :: !Int,
    -- | Cursor positioned after the current line (past its newline).
    lcRestCursor :: !Cursor,
    lcStack :: ![CondFrame],
    lcContinue :: EngineState -> Step,
    lcContinueWith :: [CondFrame] -> EngineState -> Step,
    lcDone :: Continuation
  }
