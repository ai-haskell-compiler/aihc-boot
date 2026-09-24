{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Aihc.Parser
-- Description : Haskell parser for the AIHC compiler
-- License     : Unlicense
--
-- This module provides parsing functions for Haskell source code.
-- The main entry point is 'parseModule' for parsing complete Haskell modules.
-- Additional functions are provided for parsing individual expressions,
-- patterns, and types.
module Aihc.Parser
  ( -- * Parsing modules
    parseModule,

    -- * Configuration
    ParserConfig (..),
    defaultConfig,

    -- * Parse results
    ParseResult (..),
    formatParseErrors,

    -- * Parsing expressions, patterns, types, and declarations
    parseExpr,
    parseSignatureType,
    parseType,
    parsePattern,
    parseDecl,
  )
where

import Aihc.Parser.Internal.Common (TokParser, drainParseErrors, eofTok)
import Aihc.Parser.Internal.Decl (declParser)
import Aihc.Parser.Internal.Errors (parseErrorBundleToSpannedText, parseErrorsToSpannedText)
import Aihc.Parser.Internal.Expr (exprParser)
import Aihc.Parser.Internal.Module (moduleParser)
import Aihc.Parser.Internal.Pattern (patternParser)
import Aihc.Parser.Internal.Type (typeParser, typeSignatureParser)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Decl, Expr, Extension, Module (..), Pattern, SourceSpan, Type, applyImpliedExtensions, sourceSpanEndCol, sourceSpanSourceName, sourceSpanStartCol, sourceSpanStartLine, sourceSpanStartOffset, pattern SourceSpan)
import Aihc.Parser.Types
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Prettyprinter (Doc, colon, defaultLayoutOptions, layoutPretty, pretty, vcat)
import Prettyprinter.Render.String (renderString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Aihc.Parser
-- >>> import Aihc.Parser.Syntax (moduleName)
-- >>> import Aihc.Parser.Shorthand (Shorthand(..))

-- | Default parser configuration.
--
-- * 'parserSourceName' is set to @\"\<input\>\"@
-- * 'parserExtensions' is empty (no extensions enabled by default)
--
-- >>> parserSourceName defaultConfig
-- "<input>"
--
-- >>> parserExtensions defaultConfig
-- []
defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { parserSourceName = "<input>",
      parserExtensions = []
    }

-- | Parse a Haskell expression.
--
-- >>> shorthand $ parseExpr defaultConfig "1 + 2"
-- ParseOk (EInfix (EInt 1 TInteger) "+" (EInt 2 TInteger))
--
-- >>> shorthand $ parseExpr defaultConfig "\\x -> x + 1"
-- ParseOk (ELambdaPats [PVar "x"] (EInfix (EVar "x") "+" (EInt 1 TInteger)))
--
-- Parse errors are returned as 'ParseErr':
--
-- >>> case parseExpr defaultConfig "1 +" of { ParseErr _ -> "error"; ParseOk _ -> "ok" }
-- "error"
parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr = runEntry mkTokStream (exprParser <* eofTok)

-- | Parse a Haskell pattern.
--
-- >>> shorthand $ parsePattern defaultConfig "(x, y)"
-- ParseOk (PTuple [PVar "x", PVar "y"])
--
-- >>> shorthand $ parsePattern defaultConfig "Just x"
-- ParseOk (PCon "Just" [PVar "x"])
parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern = runEntry mkTokStream (patternParser <* eofTok)

-- | Parse a Haskell signature type.
--
-- This is the context used by top-level type signatures. Unlike 'parseType',
-- it rejects an unparenthesized outer kind signature:
--
-- >>> case parseSignatureType defaultConfig "_ :: _" of { ParseErr _ -> "error"; ParseOk _ -> "ok" }
-- "error"
parseSignatureType :: ParserConfig -> Text -> ParseResult Type
parseSignatureType = runEntry mkTokStream (typeSignatureParser <* eofTok)

-- | Parse a Haskell type in the general declaration RHS context.
--
-- >>> shorthand $ parseType defaultConfig "Int -> Bool"
-- ParseOk (TFun (TCon "Int") (TCon "Bool"))
--
-- >>> shorthand $ parseType defaultConfig "Maybe a"
-- ParseOk (TApp (TCon "Maybe") (TVar "a"))
--
-- >>> shorthand $ parseType defaultConfig "_ :: _"
-- ParseOk (TKindSig (TWildcard) (TWildcard))
parseType :: ParserConfig -> Text -> ParseResult Type
parseType = runEntry mkTokStream (typeParser <* eofTok)

-- | Parse a single Haskell declaration.
--
-- >>> shorthand $ parseDecl defaultConfig "f x = x + 1"
-- ParseOk (DeclValue (FunctionBind "f" [Match {MatchHeadPrefix, [PVar "x"], EInfix (EVar "x") "+" (EInt 1 TInteger)}]))
parseDecl :: ParserConfig -> Text -> ParseResult Decl
parseDecl = runEntry mkTokStream (declParser <* eofTok)

-- | Parse a complete Haskell module.
--
-- Returns any recovered parse errors alongside a (possibly partial) 'Module'.
-- When individual declarations fail to parse, the parser recovers and continues,
-- returning the error and the successfully parsed declarations.
--
-- >>> shorthand $ snd $ parseModule defaultConfig "module Main where\nmain = putStrLn \"Hello\""
-- Module {ModuleHead {"Main"}, [DeclValue (PatternBind (PVar "main") (EApp (EVar "putStrLn") (EString "Hello")))]}
--
-- Modules without a header are also supported:
--
-- >>> case parseModule defaultConfig "x = 1" of { (_, m) -> moduleName m }
-- Nothing
parseModule :: ParserConfig -> Text -> ([(SourceSpan, Text)], Module)
parseModule cfg input =
  case runTokStreamParser parser sourceName (mkTokStreamModule sourceName exts input) of
    Left bundle ->
      ( parseErrorBundleToSpannedText sourceName errorStream bundle,
        Module
          { moduleAnns = [],
            moduleHead = Nothing,
            moduleLanguagePragmas = [],
            moduleImports = [],
            moduleDecls = []
          }
      )
    Right (errs, modu) ->
      (parseErrorsToSpannedText sourceName errorStream errs, modu)
  where
    sourceName = parserSourceName cfg
    exts = applyImpliedExtensions (parserExtensions cfg)
    errorStream = rebuildStream (\(name, es, src) -> mkTokStreamModule name es src) (sourceName, exts, input)
    parser = do
      modu <- moduleParser
      errs <- drainParseErrors
      pure (errs, modu)

-- | Run a parser over freshly lexed input. Errors are located on a stream
-- built again from the input, so the parse itself does not retain the token
-- chain (see 'rebuildStream').
runEntry :: (FilePath -> [Extension] -> Text -> TokStream) -> TokParser a -> ParserConfig -> Text -> ParseResult a
runEntry mkStream parser cfg input =
  case runTokStreamParser parser sourceName (mkStream sourceName exts input) of
    Left bundle -> ParseErr (parseErrorBundleToSpannedText sourceName (rebuildStream (\(name, es, src) -> mkStream name es src) (sourceName, exts, input)) bundle)
    Right parsed -> ParseOk parsed
  where
    sourceName = parserSourceName cfg
    exts = applyImpliedExtensions (parserExtensions cfg)

-- | Pretty-print a list of spanned parse errors with source context.
formatParseErrors :: FilePath -> Maybe Text -> [(SourceSpan, Text)] -> String
formatParseErrors sourceName mSource errs =
  let opts = defaultLayoutOptions
      blocks =
        map
          ( \(srcSpan, msg) ->
              renderString
                ( layoutPretty opts $
                    case (srcSpan, mSource) of
                      (ss, Just source) ->
                        vcat [renderSourceReference source ss, pretty msg]
                      _ ->
                        vcat [pretty sourceName, pretty msg]
                )
          )
          errs
   in List.intercalate "\n\n" blocks

-- renderSourceReference "x = 1" (SourceSpan "<input>" 1 5 1 6 4 5) = """
-- <input>:1:5:
-- 1 | x = 1
--   |     ^
-- """
-- renderSourceReference "module where" (SourceSpan "<input>" 1 8 1 13 7 12) = """
-- <input>:1:8:
-- 1 | module where
--   |        ^^^^^
-- """
renderSourceReference :: Text -> SourceSpan -> Doc ann
renderSourceReference source srcSpan =
  let SourceSpan {sourceSpanSourceName = renderedOrigin, sourceSpanStartLine = lineNo, sourceSpanStartCol = colNo, sourceSpanEndCol = endCol, sourceSpanStartOffset} = srcSpan
      srcLine = extractSourceLineByOffset source sourceSpanStartOffset
      lineNoText = show lineNo
      markerPrefix = replicate (length lineNoText) ' ' ++ " | "
      markerStart = max 0 (colNo - 1)
      markerLen = max 1 (endCol - colNo)
      marker = replicate markerStart ' ' ++ replicate markerLen '^'
      header =
        pretty renderedOrigin <> colon <> pretty lineNo <> colon <> pretty colNo <> colon
   in vcat
        [ header,
          pretty (lineNoText ++ " | " ++ srcLine),
          pretty (markerPrefix ++ marker)
        ]

extractSourceLineByOffset :: Text -> Int -> String
extractSourceLineByOffset source offset =
  let bytes = TE.encodeUtf8 source
      len = BS.length bytes
      anchor = max 0 (min len offset)
      lineStart = scanBackward bytes anchor
      lineEnd = scanForward bytes anchor
   in T.unpack (TE.decodeUtf8 (BS.take (lineEnd - lineStart) (BS.drop lineStart bytes)))

scanBackward :: BS.ByteString -> Int -> Int
scanBackward bytes = go
  where
    go idx
      | idx <= 0 = 0
      | isLineBreak (BS.index bytes (idx - 1)) = idx
      | otherwise = go (idx - 1)

scanForward :: BS.ByteString -> Int -> Int
scanForward bytes = go
  where
    len = BS.length bytes
    go idx
      | idx >= len = len
      | isLineBreak (BS.index bytes idx) = idx
      | otherwise = go (idx + 1)

isLineBreak :: Word8 -> Bool
isLineBreak w = w == 10 || w == 13
