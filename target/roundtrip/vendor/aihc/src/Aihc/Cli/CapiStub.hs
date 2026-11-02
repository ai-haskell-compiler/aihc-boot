module Aihc.Cli.CapiStub (CapiStubOptions(..), noCapiStubOptions, capiStubArguments) where {
import Aihc.Native (NativeTarget(..), OptimizationLevel, WasmSysroot(..), backendCompiler, handwrittenCArguments, wasmSysroot);
data CapiStubOptions = CapiStubOptions { capiStubIncludeDirs :: ![FilePath], capiStubCcOptions :: ![String] } deriving (Eq, Show);
noCapiStubOptions :: CapiStubOptions;
noCapiStubOptions = CapiStubOptions [] [];
capiStubArguments :: NativeTarget -> OptimizationLevel -> CapiStubOptions -> FilePath -> IO [String];
capiStubArguments target level options headerDirectory = do {
  (_, targetArguments) <- backendCompiler target;
  sysrootIncludes <- case target of {
  Wasm32Wasip3 -> do {
  sysroot <- wasmSysroot;
  pure ["-isystem" <> wasmSysrootInclude sysroot]};
  _ -> pure []};
  pure (targetArguments <> handwrittenCArguments level <> capiStubCcOptions options <> sysrootIncludes <> ["-I" <> directory | directory <- capiStubIncludeDirs options] <> ["-I" <> headerDirectory])}
}
