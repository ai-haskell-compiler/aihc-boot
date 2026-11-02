module Aihc.Wasm (wasip3WorldPath) where {
import Aihc.DataFiles (getDataFileName);
wasip3WorldPath :: IO FilePath;
wasip3WorldPath = getDataFileName "compiler/wasm/runtime/wit"
}
