{-# LANGUAGE RankNTypes #-}

-- | Parse Haskell files with aihc-parser and compare the syntax trees.
--
-- aihc-parser is the parser that aihc itself uses, vendored under
-- vendor/aihc-parser. It is the reference for aihc-boot: a module counts
-- as parsed when aihc-parser's tree for the original file is equal to its
-- tree for the file that aihc-boot printed from its own syntax tree.
--
-- Usage:
--
-- > aihc-parse [-XExtension ...] FILE          print the syntax tree
-- > aihc-parse [-XExtension ...] FILE1 FILE2   exit 0 if the trees are equal
--
-- `LANGUAGE` pragmas in each file apply on top of the `-X` flags.
--
-- The comparison ignores source spans and pragmas. aihc-boot ignores
-- pragmas, because they do not change what the vendored code computes.
module Main (main) where

import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Shorthand (shorthand)
import Aihc.Parser.Syntax
import Control.Monad (unless)
import Data.Data (Data, Typeable, cast, gmapT)
import Data.List (isPrefixOf, partition)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let (flags, files) = partition ("-X" `isPrefixOf`) args
  settings <- mapM extension flags
  let config = defaultConfig {parserExtensions = foldl' apply [] settings}
      apply exts (EnableExtension ext) = exts ++ [ext]
      apply exts (DisableExtension ext) = filter (/= ext) exts
  case files of
    [file] -> do
      tree <- parseFile config file
      print (shorthand tree)
    [a, b] -> do
      ta <- parseFile config a
      tb <- parseFile config b
      unless (ta == tb) $ do
        hPutStrLn stderr ("aihc-parse: the syntax trees of " ++ a ++ " and " ++ b ++ " differ")
        writeFile (b ++ ".aihc-parse.original") (show (shorthand ta))
        writeFile (b ++ ".aihc-parse.printed") (show (shorthand tb))
        exitFailure
    _ -> do
      hPutStrLn stderr "usage: aihc-parse [-XExtension ...] FILE [FILE2]"
      exitFailure

-- | A @-X@ flag. @-XNoName@ turns an extension off.
extension :: String -> IO ExtensionSetting
extension flag =
  case parseExtensionSettingName (T.pack (drop 2 flag)) of
    Just ext -> pure ext
    Nothing -> do
      hPutStrLn stderr ("aihc-parse: unknown extension " ++ flag)
      exitFailure

-- | Parse a file. Exit with the parse errors when there are any.
parseFile :: ParserConfig -> FilePath -> IO Module
parseFile config file = do
  input <- TIO.readFile file
  let (errors, tree) = parseModule config {parserSourceName = file} input
  unless (null errors) $ do
    hPutStrLn stderr (formatParseErrors file (Just input) errors)
    hPutStrLn stderr ("aihc-parse: " ++ file ++ " does not parse")
    exitFailure
  pure (stripPragmas (stripAnnotations tree))

-- | Apply a transformation everywhere in a tree, bottom up.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f x = f (gmapT (everywhere f) x)

-- | Lift a transformation on one type to every type.
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f x = fromMaybe x (cast . f =<< cast x)

-- | Remove every pragma from the tree. `stripAnnotations` has already
-- removed the annotation wrappers, so the pragma items are bare.
stripPragmas :: Data a => a -> a
stripPragmas =
  everywhere
    ( mkT (const [] :: [Pragma] -> [Pragma])
        . mkT (const Nothing :: Maybe Pragma -> Maybe Pragma)
        . mkT (filter (not . isDeclPragma))
        . mkT (filter (not . isClassPragma))
        . mkT (filter (not . isInstancePragma))
        . mkT stripExprPragma
    )
  where
    isDeclPragma :: Decl -> Bool
    isDeclPragma (DeclPragma _) = True
    isDeclPragma _ = False

    isClassPragma :: ClassDeclItem -> Bool
    isClassPragma (ClassItemPragma _) = True
    isClassPragma _ = False

    isInstancePragma :: InstanceDeclItem -> Bool
    isInstancePragma (InstanceItemPragma _) = True
    isInstancePragma _ = False

    stripExprPragma :: Expr -> Expr
    stripExprPragma (EPragma _ e) = e
    stripExprPragma e = e
