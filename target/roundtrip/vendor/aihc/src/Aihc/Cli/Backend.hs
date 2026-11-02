module Aihc.Cli.Backend (BackendOutput(..), compileLirWith, compileLirTo, compileLirObject, compileEntryObject, compileGrinTo, lirModuleDefinesCode, lowerTargetFor, nativeSourceExtension, nativeSourceIsLir) where {
import Aihc.Amd64.Lir qualified as Amd64;
import Aihc.Arm64.Lir qualified as Arm64;
import Aihc.Grin.Gc (GcGrinProgram);
import Aihc.Lir.Lower (LowerTarget, appleArm64Target, posixTarget64, wasip3Target);
import Aihc.Lir.Lower qualified as Lower;
import Aihc.Lir.Pretty (renderModule);
import Aihc.Lir.Syntax (Item(..), Module(..));
import Aihc.Llvm.Lir qualified as Llvm;
import Aihc.Native (NativeTarget(..), backendCompiler, optimizationArgument, runtimeOptimizationLevel);
import Aihc.Wasm.Lir qualified as Wasm;
import Data.ByteString.Lazy qualified as BL;
import Data.Text (Text);
import Data.Text.IO qualified as TIO;
import System.Exit (ExitCode(..));
import System.FilePath (takeBaseName, (</>));
import System.Process (readProcessWithExitCode);
data BackendOutput = BackendObject !BL.ByteString | BackendSource !Text;
lowerTargetFor :: NativeTarget -> LowerTarget;
lowerTargetFor target = case target of {
  AppleArm64 -> appleArm64Target;
  Wasm32Wasip3 -> wasip3Target;
  _ -> posixTarget64};
compileLirWith :: Bool -> NativeTarget -> Module -> Either String BackendOutput;
compileLirWith lint target lirModule = case target of {
  AppleArm64 -> either (Left . show) (Right . BackendObject) (Arm64.compileLirObjectWith lint lirModule);
  LinuxAmd64 -> either (Left . show) (Right . BackendObject) (Amd64.compileLirObjectWith lint lirModule);
  Llvm -> either (Left . show) (Right . BackendSource) (Llvm.compileLirModule lirModule);
  Wasm32Wasip3 -> either (Left . show) (Right . BackendSource) (Wasm.compileLirModule lirModule)};
compileLirTo :: Bool -> NativeTarget -> Module -> FilePath -> IO (Maybe Text);
compileLirTo lint target lirModule path = case target of {
  AppleArm64 -> Arm64.writeLirObjectWith lint lirModule path >> pure Nothing;
  LinuxAmd64 -> Amd64.writeLirObjectWith lint lirModule path >> pure Nothing;
  _ -> do {
  output <- either (ioError . userError . ("Lir backend failed: " <>)) pure (compileLirWith lint target lirModule);
  case output of {
  BackendObject bytes -> BL.writeFile path bytes >> pure Nothing;
  BackendSource source -> pure (Just source)}}};
compileLirObject :: NativeTarget -> String -> Module -> FilePath -> FilePath -> IO ();
compileLirObject target name lirModule directory object = do {
  output <- compileLirTo True target lirModule object;
  case output of {
  Nothing -> pure ();
  Just source -> do {
  let {
  sourcePath = directory </> name <> nativeSourceExtension target};
  TIO.writeFile sourcePath source;
  (compiler, arguments) <- backendCompiler target;
  let {
  optimizationArguments = [optimizationArgument runtimeOptimizationLevel | target == Llvm]};
  (exitCode, _stdout, stderr) <- readProcessWithExitCode compiler (arguments <> optimizationArguments <> ["-c", sourcePath, "-o", object]) "";
  case exitCode of {
  ExitSuccess -> pure ();
  ExitFailure _ -> ioError (userError (compiler <> " failed (" <> show exitCode <> "): " <> stderr))}}}};
lirModuleDefinesCode :: Module -> Bool;
lirModuleDefinesCode lirModule = any definesCode (moduleItems lirModule) where {
  definesCode item = case item of {
  ItemFunction _ -> True;
  ItemGlobal _ -> True;
  ItemData _ -> True;
  ItemExternFunction _ -> False;
  ItemExternData _ -> False;
  ItemConstant _ -> False;
  ItemInclude _ -> False}};
compileEntryObject :: NativeTarget -> FilePath -> FilePath -> IO ();
compileEntryObject target directory object = do {
  entryModule <- either (ioError . userError . ("Lir entry generation failed: " <>) . show) pure (Lower.lowerEntry (lowerTargetFor target));
  compileLirObject target (takeBaseName object) entryModule directory object};
compileGrinTo :: Bool -> Bool -> NativeTarget -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO (Maybe Text);
compileGrinTo lint checkBounds target lirPath gcProgram path = case target of {
  AppleArm64 -> Arm64.writeGrinObjectWith lint checkBounds lirPath gcProgram path >> pure Nothing;
  LinuxAmd64 -> Amd64.writeGrinObjectWith lint checkBounds lirPath gcProgram path >> pure Nothing;
  _ -> do {
  lirModule <- either (ioError . userError . ("Lir generation failed: " <>) . show) pure (Lower.lowerModule (lowerTargetFor target) checkBounds gcProgram);
  mapM_ (\dump -> TIO.writeFile dump (renderLirModule lirModule)) lirPath;
  compileLirTo lint target lirModule path}};
renderLirModule :: Module -> Text;
renderLirModule lirModule = foldMap (\item -> renderModule (Module [item]) <> "\n") (moduleItems lirModule);
nativeSourceExtension :: NativeTarget -> String;
nativeSourceExtension target = case target of {
  AppleArm64 -> ".lir";
  LinuxAmd64 -> ".lir";
  Llvm -> ".ll";
  Wasm32Wasip3 -> ".s"};
nativeSourceIsLir :: NativeTarget -> Bool;
nativeSourceIsLir target = case target of {
  AppleArm64 -> True;
  LinuxAmd64 -> True;
  Llvm -> False;
  Wasm32Wasip3 -> False}
}
