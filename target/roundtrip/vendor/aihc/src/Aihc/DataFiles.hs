module Aihc.DataFiles (getDataFileName) where {
import Paths_aihc qualified;
getDataFileName :: FilePath -> IO FilePath;
getDataFileName = Paths_aihc.getDataFileName
}
