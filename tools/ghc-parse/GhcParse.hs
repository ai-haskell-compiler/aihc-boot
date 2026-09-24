{-# LANGUAGE RankNTypes #-}

-- | Parse Haskell files with GHC's parser and print the syntax tree.
--
-- The boot compiler uses this tool as its reference: a module counts as
-- parsed when GHC's tree for the original file is equal to GHC's tree for
-- the file that aihc-boot printed from its own syntax tree.
--
-- The tool removes pragmas from the tree before it prints: `INLINE`,
-- `SPECIALIZE`, `UNPACK`, `SCC`, `SOURCE` and the others. aihc-boot
-- ignores them, because they do not change what the vendored code
-- computes.
--
-- Usage:
--
-- > ghc-parse [-XExtension ...] FILE          print GHC's syntax tree
-- > ghc-parse [-XExtension ...] FILE1 FILE2   exit 0 if the trees are equal
--
-- `LANGUAGE` pragmas in each file apply on top of the `-X` flags.
module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Data, Typeable, cast, gmapT)
import Data.List (isPrefixOf, partition)
import Data.Maybe (fromMaybe)
import GHC hiding (parseModule)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (hGetStringBuffer)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPsMessageOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors (printMessages)
import GHC.Driver.Session (parseDynamicFilePragma, parseDynamicFlagsCmdLine)
import GHC.Parser (parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (..), ParseResult (..), getPsErrorMessages, initParserState)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Logger (getLogger)
import GHC.Utils.Outputable (ppr)
import GHC.Driver.Ppr (showSDoc)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  let (flags, files) = partition ("-X" `isPrefixOf`) args
  libdir <- takeWhile (/= '\n') <$> readProcess "ghc" ["--print-libdir"] ""
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    (dflags1, _, _) <- parseDynamicFlagsCmdLine dflags0 (map noLoc flags)
    case files of
      [file] -> do
        tree <- parseFile dflags1 file
        liftIO (putStrLn tree)
      [a, b] -> do
        ta <- parseFile dflags1 a
        tb <- parseFile dflags1 b
        unless (ta == tb) $ liftIO $ do
          hPutStrLn stderr ("ghc-parse: the syntax trees of " ++ a ++ " and " ++ b ++ " differ")
          writeFile (b ++ ".ghc-parse.original") ta
          writeFile (b ++ ".ghc-parse.printed") tb
          exitFailure
      _ -> liftIO $ do
        hPutStrLn stderr "usage: ghc-parse [-XExtension ...] FILE [FILE2]"
        exitFailure

parseFile :: DynFlags -> FilePath -> Ghc String
parseFile dflags file = do
  buf <- liftIO (hGetStringBuffer file)
  let (_, opts) = getOptions (initParserOpts dflags) buf file
  (dflags', _, _) <- parseDynamicFilePragma dflags opts
  let loc = mkRealSrcLoc (mkFastString file) 1 1
      state = initParserState (initParserOpts dflags') buf loc
  case unP parseModule state of
    POk _ tree -> pure (showSDoc dflags' (ppr (stripPragmas tree)))
    PFailed failed -> do
      logger <- getLogger
      liftIO $ do
        printMessages logger (initPsMessageOpts dflags') (initDiagOpts dflags') (getPsErrorMessages failed)
        hPutStrLn stderr ("ghc-parse: " ++ file ++ " does not parse")
        exitFailure

-- | Apply a transformation everywhere in a tree, bottom up.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f x = f (gmapT (everywhere f) x)

-- | Lift a transformation on one type to every type.
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f x = fromMaybe x (cast . f =<< cast x)

stripPragmas :: Data a => a -> a
stripPragmas =
  everywhere
    ( mkT stripDecls
        . mkT stripSigs
        . mkT stripBang
        . mkT stripExpr
        . mkT stripOverlap
        . mkT stripSource
    )

stripDecls :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
stripDecls = filter (not . isPragmaDecl . unLoc)
  where
    isPragmaDecl (SigD _ sig) = isPragmaSig sig
    isPragmaDecl (RuleD {}) = True
    isPragmaDecl (WarningD {}) = True
    isPragmaDecl (AnnD {}) = True
    isPragmaDecl _ = False

stripSigs :: [LSig GhcPs] -> [LSig GhcPs]
stripSigs = filter (not . isPragmaSig . unLoc)

isPragmaSig :: Sig GhcPs -> Bool
isPragmaSig sig = case sig of
  InlineSig {} -> True
  SpecSig {} -> True
  SpecInstSig {} -> True
  MinimalSig {} -> True
  SCCFunSig {} -> True
  CompleteMatchSig {} -> True
  _ -> False

stripBang :: HsBang -> HsBang
stripBang (HsBang _ strictness) = HsBang NoSrcUnpack strictness

stripExpr :: HsExpr GhcPs -> HsExpr GhcPs
stripExpr (HsPragE _ _ (L _ e)) = e
stripExpr e = e

stripSource :: ImportDecl GhcPs -> ImportDecl GhcPs
stripSource decl = decl {ideclSource = NotBoot}

stripOverlap :: ClsInstDecl GhcPs -> ClsInstDecl GhcPs
stripOverlap decl = decl {cid_overlap_mode = Nothing}
