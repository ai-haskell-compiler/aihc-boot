module Aihc.Cli.Store (defaultStoreRoot) where {
import System.Directory (XdgDirectory(XdgCache), getXdgDirectory);
import System.FilePath ((</>));
defaultStoreRoot :: IO FilePath;
defaultStoreRoot = do {
  cacheDirectory <- getXdgDirectory XdgCache "aihc";
  pure (cacheDirectory </> "store")}
}
