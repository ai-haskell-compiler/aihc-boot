module System.FilePath.Posix (FilePath, takeDirectory, combine, (</>)) where {
import Prelude (Char, Bool(..), (.), (&&), not, fst, maybe, (||), (==), ($), otherwise, snd);
import Data.Semigroup ((<>));
import qualified Data.List as L;
import Prelude (String, FilePath, last, null, span);
import Data.List (uncons, dropWhileEnd);
pathSeparator :: Char;
pathSeparator = _slash;
isPathSeparator :: Char -> Bool;
isPathSeparator c = c == _slash;
splitDrive :: FilePath -> (FilePath, FilePath);
splitDrive x = span (== _slash) x;
takeDrive :: FilePath -> FilePath;
takeDrive = fst . splitDrive;
dropDrive :: FilePath -> FilePath;
dropDrive = snd . splitDrive;
hasDrive :: FilePath -> Bool;
hasDrive = not . null . takeDrive;
isDrive :: FilePath -> Bool;
isDrive x = not (null x) && null (dropDrive x);
splitFileName :: FilePath -> (String, String);
splitFileName x = if null path then (dotSlash, file) else (path, file) where {
  (path, file) = splitFileName_ x;
  dotSlash = _period `cons` singleton _slash};
splitFileName_ :: FilePath -> (String, String);
splitFileName_ fp = (dirSlash, file) where {
  (dirSlash, file) = breakEnd isPathSeparator fp};
dropFileName :: FilePath -> FilePath;
dropFileName = fst . splitFileName;
hasTrailingPathSeparator :: FilePath -> Bool;
hasTrailingPathSeparator x | null x = False | otherwise = isPathSeparator $ last x;
hasLeadingPathSeparator :: FilePath -> Bool;
hasLeadingPathSeparator = maybe False (isPathSeparator . fst) . uncons;
dropTrailingPathSeparator :: FilePath -> FilePath;
dropTrailingPathSeparator x = if hasTrailingPathSeparator x && not (isDrive x) then let {
  x' = dropWhileEnd isPathSeparator x} in if null x' then singleton (last x) else x' else x;
takeDirectory :: FilePath -> FilePath;
takeDirectory = dropTrailingPathSeparator . dropFileName;
combine :: FilePath -> FilePath -> FilePath;
combine a b | hasLeadingPathSeparator b || hasDrive b = b | otherwise = combineAlways a b;
combineAlways :: FilePath -> FilePath -> FilePath;
combineAlways a b | null a = b | null b = a | hasTrailingPathSeparator a = a <> b | otherwise = a <> (pathSeparator `cons` b);
(</>) :: FilePath -> FilePath -> FilePath;
(</>) = combine;
spanEnd :: (a -> Bool) -> [a] -> ([a], [a]);
spanEnd p = L.foldr (\x (pref, suff) -> if null pref && p x then (pref, x : suff) else (x : pref, suff)) ([], []);
breakEnd :: (a -> Bool) -> [a] -> ([a], [a]);
breakEnd p = spanEnd (not . p);
cons :: a -> [a] -> [a];
cons = (:);
_period, _slash :: Char;
_period = '.';
_slash = '/';
singleton :: Char -> String;
singleton c = [c]
}
