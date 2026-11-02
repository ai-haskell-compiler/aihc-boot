{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cpp.Types (Config(..), MacroDef(..), defaultConfig, IncludeKind(..), IncludeRequest(..), Severity(..), Diagnostic(..), Result(..), Step(..), EngineState(..), emptyState, defineMacro, undefMacro, setMacroTable, macroFirstByte, bloomMember, CondFrame(..), currentActive, mkFrame, Continuation, LineContext(..)) where {
import Aihc.Cpp.Cursor (Cursor);
import Control.DeepSeq (NFData);
import Data.Bits (setBit, testBit, (.&.));
import Data.ByteString (ByteString);
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Builder as BSB;
import Data.Map.Strict (Map);
import qualified Data.Map.Strict as M;
import Data.Set (Set);
import qualified Data.Set as S;
import Data.Text (Text);
import Data.Word (Word64, Word8);
import GHC.Generics (Generic);
data Config = Config { configInputFile :: FilePath, configMacros :: !(Map ByteString ByteString) };
data MacroDef = ObjectMacro !ByteString | FunctionMacro ![ByteString] !ByteString deriving (Eq, Show);
defaultConfig :: Config;
defaultConfig = Config {configInputFile = "<input>", configMacros = M.fromList [("__DATE__", "\"Jan  1 1970\""), ("__TIME__", "\"00:00:00\"")]};
data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show, Generic, NFData);
data IncludeRequest = IncludeRequest { includePath :: !FilePath, includeKind :: !IncludeKind, includeFrom :: !FilePath, includeLine :: !Int } deriving (Eq, Show, Generic, NFData);
data Severity = Warning | Error deriving (Eq, Show, Generic, NFData);
data Diagnostic = Diagnostic { diagSeverity :: !Severity, diagMessage :: !Text, diagFile :: !FilePath, diagLine :: !Int } deriving (Eq, Show, Generic, NFData);
data Result = Result { resultOutput :: !ByteString, resultDiagnostics :: ![Diagnostic] } deriving (Eq, Show, Generic, NFData);
data Step = Done !Result | NeedInclude !IncludeRequest !(Maybe ByteString -> Step);
data EngineState = EngineState { stMacros :: !(Map ByteString MacroDef), stMacroBloom :: !Word64, stFunMacroBloom :: !Word64, stOutput :: !BSB.Builder, stOutputLineCount :: !Int, stDiagnosticsRev :: ![Diagnostic], stPragmaOnceFiles :: !(Set FilePath), stSkippingDanglingElse :: !Bool, stHsBlockCommentDepth :: !Int, stCBlockCommentDepth :: !Int, stCurrentFile :: !FilePath, stCurrentLine :: !Int };
emptyState :: FilePath -> EngineState;
emptyState filePath = EngineState {stMacros = M.empty, stMacroBloom = underscoreBloom, stFunMacroBloom = 0, stOutput = mempty, stOutputLineCount = 0, stDiagnosticsRev = [], stPragmaOnceFiles = S.empty, stSkippingDanglingElse = False, stHsBlockCommentDepth = 0, stCBlockCommentDepth = 0, stCurrentFile = filePath, stCurrentLine = 1};
defineMacro :: ByteString -> MacroDef -> EngineState -> EngineState;
defineMacro name def st = st {stMacros = M.insert name def (stMacros st), stMacroBloom = setBit (stMacroBloom st) bit', stFunMacroBloom = case def of {
  FunctionMacro _ _ -> setBit (stFunMacroBloom st) bit';
  ObjectMacro _ -> stFunMacroBloom st}} where {
  bit' = macroFirstByte name};
undefMacro :: ByteString -> EngineState -> EngineState;
undefMacro name st = setMacroTable (M.delete name (stMacros st)) st;
setMacroTable :: Map ByteString MacroDef -> EngineState -> EngineState;
setMacroTable macros st = let {
  (allBloom, funBloom) = M.foldrWithKey step (underscoreBloom, 0) macros} in st {stMacros = macros, stMacroBloom = allBloom, stFunMacroBloom = funBloom} where {
  step name def (allBloom, funBloom) = let {
  bit' = macroFirstByte name;
  allBloom' = setBit allBloom bit'} in case def of {
  FunctionMacro _ _ -> (allBloom', setBit funBloom bit');
  ObjectMacro _ -> (allBloom', funBloom)}};
underscoreBloom :: Word64;
underscoreBloom = setBit 0 (macroFirstByte "_");
macroFirstByte :: ByteString -> Int;
macroFirstByte name | BS.null name = 0 | otherwise = fromIntegral (BS.head name .&. 0x3F);
bloomMember :: Word64 -> Word8 -> Bool;
bloomMember bloom b = testBit bloom (fromIntegral (b .&. 0x3F));
data CondFrame = CondFrame { frameOuterActive :: !Bool, frameConditionTrue :: !Bool, frameInElse :: !Bool, frameCurrentActive :: !Bool };
currentActive :: [CondFrame] -> Bool;
currentActive [] = True;
currentActive (f : _) = frameCurrentActive f;
mkFrame :: Bool -> Bool -> CondFrame;
mkFrame outer cond = CondFrame {frameOuterActive = outer, frameConditionTrue = cond, frameInElse = False, frameCurrentActive = outer && cond};
type Continuation = EngineState -> Step;
data LineContext = LineContext { lcFilePath :: !FilePath, lcLineNo :: !Int, lcLineSpan :: !Int, lcNextLineNo :: !Int, lcRestCursor :: !Cursor, lcStack :: ![CondFrame], lcContinue :: EngineState -> Step, lcContinueWith :: [CondFrame] -> EngineState -> Step, lcDone :: Continuation }
}
