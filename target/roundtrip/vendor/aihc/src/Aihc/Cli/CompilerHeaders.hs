module Aihc.Cli.CompilerHeaders (cabalPlatformForTarget, hostPlatformMacros, headerTargetFor, compilerHeaderIdentity, ensureCompilerHeaders) where {
import Aihc.Cli.ArtifactCache (hashChunks);
import Aihc.Hackage.Headers (HeaderTarget(..), compilerHeaderTexts, writeCompilerHeaders);
import Aihc.Native (NativeTarget(..));
import Data.Text qualified as T;
import Data.Text.Encoding qualified as TE;
import Distribution.Pretty (prettyShow);
import Distribution.System (Arch(..), OS(..), buildArch, buildOS);
cabalPlatformForTarget :: NativeTarget -> (OS, Arch);
cabalPlatformForTarget target = case target of {
  AppleArm64 -> (OSX, AArch64);
  LinuxAmd64 -> (Linux, X86_64);
  Llvm -> (buildOS, buildArch);
  Wasm32Wasip3 -> (Wasi, Wasm32)};
hostPlatformMacros :: NativeTarget -> [String];
hostPlatformMacros target = let {
  (os, arch) = platformNames target} in ["-D" <> os <> "_HOST_OS=1", "-D" <> arch <> "_HOST_ARCH=1"];
platformNames :: NativeTarget -> (String, String);
platformNames target = let {
  (os, arch) = cabalPlatformForTarget target;
  osName = case os of {
  OSX -> "darwin";
  other -> prettyShow other}} in (osName, prettyShow arch);
headerTargetFor :: NativeTarget -> HeaderTarget;
headerTargetFor target = HeaderTarget {headerPointerBytes = case target of {
  Wasm32Wasip3 -> 4;
  _ -> 8}, headerLongBytes = case target of {
  Wasm32Wasip3 -> 4;
  _ -> 8}, headerBigEndian = False, headerOs = T.pack os, headerArch = T.pack arch} where {
  (os, arch) = platformNames target};
compilerHeaderIdentity :: NativeTarget -> String;
compilerHeaderIdentity target = hashChunks (concat [[TE.encodeUtf8 (T.pack path), TE.encodeUtf8 text] | (path, text) <- compilerHeaderTexts (headerTargetFor target)]);
ensureCompilerHeaders :: NativeTarget -> FilePath -> IO FilePath;
ensureCompilerHeaders target = writeCompilerHeaders (headerTargetFor target)
}
