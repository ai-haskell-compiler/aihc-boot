-- |
-- Module      :  System.FilePath.Posix
-- Copyright   :  (c) Neil Mitchell 2005-2014
-- License     :  BSD3
--
-- Maintainer  :  ndmitchell@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- A library for 'FilePath' manipulations, using Posix style paths on
-- all platforms. Importing "System.FilePath" is usually better.
--
-- aihc-boot keeps only the parts that the vendored tree uses. Upstream
-- generates this module and the Windows module from one template. Here
-- @isWindows@ is always 'False', so the Windows branches are gone.
module System.FilePath.Posix
    (
    -- * Separator predicates
    FilePath,

    -- * Directory functions
    takeDirectory,

    -- * File name manipulations
    combine, (</>),
    )
    where

import Prelude (Char, Bool(..), (.), (&&), not, fst, maybe, (||), (==), ($), otherwise, snd)
import Data.Semigroup ((<>))
import qualified Data.List as L
import Prelude (String, FilePath, last, null, span)
import Data.List(uncons, dropWhileEnd)

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator = _slash

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator c = c == _slash

-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (<>) (splitDrive x) == x
-- > splitDrive "/test" == ("/","test")
-- > splitDrive "//test" == ("//","test")
-- > splitDrive "test/file" == ("","test/file")
-- > splitDrive "file" == ("","file")
splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x = span (== _slash) x

-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FilePath -> FilePath
takeDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > hasDrive "/foo" == True
-- > hasDrive "foo" == False
-- > hasDrive "" == False
hasDrive :: FilePath -> Bool
hasDrive = not . null . takeDrive

-- | Is an element a drive
--
-- > isDrive "/" == True
-- > isDrive "/foo" == False
-- > isDrive "" == False
isDrive :: FilePath -> Bool
isDrive x = not (null x) && null (dropDrive x)

-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > splitFileName "/" == ("/","")
splitFileName :: FilePath -> (String, String)
splitFileName x = if null path
    then (dotSlash, file)
    else (path, file)
  where
    (path, file) = splitFileName_ x
    dotSlash = _period `cons` singleton _slash

-- version of splitFileName where, if the FilePath has no directory
-- component, the returned directory is "" rather than "./".  This
-- is used in cases where we are going to combine the returned
-- directory to make a valid FilePath, and having a "./" appear would
-- look strange and upset simple equality properties.  See
-- e.g. replaceFileName.
--
-- Upstream handles Windows drive and UNC names here. On Posix the
-- split at the last path separator is already the answer.
splitFileName_ :: FilePath -> (String, String)
splitFileName_ fp = (dirSlash, file)
  where
    (dirSlash, file) = breakEnd isPathSeparator fp

-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
-- > isPrefixOf (takeDrive x) (dropFileName x)
dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName

-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator x
  | null x = False
  | otherwise = isPathSeparator $ last x

hasLeadingPathSeparator :: FilePath -> Bool
hasLeadingPathSeparator = maybe False (isPathSeparator . fst) . uncons

-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator x =
    if hasTrailingPathSeparator x && not (isDrive x)
    then let x' = dropWhileEnd isPathSeparator x
         in if null x' then singleton (last x) else x'
    else x

-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           isPrefixOf (takeDirectory x) x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
takeDirectory :: FilePath -> FilePath
takeDirectory = dropTrailingPathSeparator . dropFileName

-- | An alias for '</>'.
combine :: FilePath -> FilePath -> FilePath
combine a b | hasLeadingPathSeparator b || hasDrive b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b | null a = b
                  | null b = a
                  | hasTrailingPathSeparator a = a <> b
                  | otherwise = a <> (pathSeparator `cons` b)

-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > "/directory" </> "file.ext" == "/directory/file.ext"
-- > "directory" </> "/file.ext" == "/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > "/" </> "test" == "/test"
-- > "home" </> "bob" == "home/bob"
-- > "x:" </> "foo" == "x:/foo"
--
--   Not combined:
--
-- > "home" </> "/bob" == "/bob"
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

-----------------------------------------------------------------------------
-- spanEnd (>2) [1,2,3,4,1,2,3,4] = ([1,2,3,4,1,2], [3,4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = L.foldr (\x (pref, suff) -> if null pref && p x then (pref, x : suff) else (x : pref, suff)) ([], [])

-- breakEnd (< 2) [1,2,3,4,1,2,3,4] == ([1,2,3,4,1],[2,3,4])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

cons :: a -> [a] -> [a]
cons = (:)

_period, _slash :: Char
_period = '.'
_slash = '/'

singleton :: Char -> String
singleton c = [c]
