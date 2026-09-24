{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Cpp.Parser
-- Description : Directive parsing over raw bytes
-- License     : Unlicense
--
-- Directives are parsed straight from the input bytes. Every character
-- that is significant to the C preprocessor is ASCII, so no decoding is
-- required; bytes >= 0x80 are only ever carried along inside identifiers,
-- macro bodies and message text.
module Aihc.Cpp.Parser
  ( Directive (..),
    parseDirective,
    parseDirectiveBody,
    parseDefine,
    parseInclude,
    parseLineDirective,
    parseIdentifier,
    parseQuoted,
    parseDefineParams,
    isIdentStart,
    isIdentChar,
    isOpChar,
    isSpaceChar,
    strip,
    stripStart,
  )
where

import Aihc.Cpp.Types (IncludeKind (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

data Directive
  = DirDefineObject !ByteString !ByteString
  | DirDefineFunction !ByteString ![ByteString] !ByteString
  | DirUndef !ByteString
  | DirInclude !IncludeKind !ByteString
  | DirIf !ByteString
  | DirIfDef !ByteString
  | DirIfNDef !ByteString
  | DirElif !ByteString
  | DirElse
  | DirEndIf
  | DirLine !Int !(Maybe FilePath)
  | DirPragmaOnce
  | DirWarning !ByteString
  | DirError !ByteString
  | DirUnsupported !ByteString

parseDirective :: ByteString -> Maybe Directive
parseDirective raw =
  let trimmed = stripStart raw
   in if "#" `C.isPrefixOf` trimmed
        then
          let body = stripStart (C.drop 1 trimmed)
           in case C.uncons body of
                Just (c, _) | isIdentStart c || isDigit c -> parseDirectiveBody body
                _ -> Nothing
        else Nothing

parseDirectiveBody :: ByteString -> Maybe Directive
parseDirectiveBody body =
  let (name, rest0) = C.span isIdentChar body
      rest = stripStart rest0
   in if C.null name
        then case C.uncons body of
          Just (c, _) | isDigit c -> parseLineDirective body
          _ -> Nothing
        else case name of
          "define" -> parseDefine rest
          "undef" -> DirUndef <$> parseIdentifier rest
          "include" -> parseInclude rest
          "if" -> Just (DirIf rest)
          "ifdef" -> DirIfDef <$> parseIdentifier rest
          "ifndef" -> DirIfNDef <$> parseIdentifier rest
          -- Keep `#isndef` as explicitly unsupported for diagnostics on common typo input.
          "isndef" -> Just (DirUnsupported "isndef")
          "elif" -> Just (DirElif rest)
          "elseif" -> Just (DirElif rest)
          "else" -> Just DirElse
          "endif" -> Just DirEndIf
          "line" -> parseLineDirective rest
          "pragma" -> parsePragma rest
          "warning" -> Just (DirWarning rest)
          "error" -> Just (DirError rest)
          _ -> Nothing

parseLineDirective :: ByteString -> Maybe Directive
parseLineDirective body =
  case C.uncons body of
    -- 'C.readInt' also accepts a leading sign; a #line number must not.
    Just (c, _) | isDigit c ->
      case C.readInt body of
        Nothing -> Nothing
        Just (lineNumber, rest0) ->
          let rest = stripStart rest0
           in case parseQuoted rest of
                Nothing -> Just (DirLine lineNumber Nothing)
                Just path -> Just (DirLine lineNumber (Just (C.unpack path)))
    _ -> Nothing

parsePragma :: ByteString -> Maybe Directive
parsePragma body =
  if strip body == "once" then Just DirPragmaOnce else Nothing

parseDefine :: ByteString -> Maybe Directive
parseDefine rest = do
  let (name, rest0) = C.span isIdentChar rest
  if C.null name
    then Nothing
    else case C.uncons rest0 of
      Just ('(', afterOpen) ->
        let (params, restAfterParams) = parseDefineParams afterOpen
         in case params of
              Nothing -> Just (DirUnsupported "define-function-macro")
              Just names -> Just (DirDefineFunction name names (stripStart restAfterParams))
      _ -> Just (DirDefineObject name (stripStart rest0))

parseDefineParams :: ByteString -> (Maybe [ByteString], ByteString)
parseDefineParams input =
  let (inside, suffix) = C.break (== ')') input
   in if C.null suffix
        then (Nothing, "")
        else
          let rawParams = C.split ',' inside
              params = map (C.takeWhile isIdentChar . strip) rawParams
           in if C.null (strip inside)
                then (Just [], C.drop 1 suffix)
                else
                  if any C.null params
                    then (Nothing, C.drop 1 suffix)
                    else (Just params, C.drop 1 suffix)

parseIdentifier :: ByteString -> Maybe ByteString
parseIdentifier txt =
  let ident = C.takeWhile isIdentChar (stripStart txt)
   in if C.null ident then Nothing else Just ident

parseInclude :: ByteString -> Maybe Directive
parseInclude txt =
  case C.uncons (stripStart txt) of
    Just ('"', rest) ->
      let (path, suffix) = C.break (== '"') rest
       in if C.null suffix then Nothing else Just (DirInclude IncludeLocal path)
    Just ('<', rest) ->
      let (path, suffix) = C.break (== '>') rest
       in if C.null suffix then Nothing else Just (DirInclude IncludeSystem path)
    _ -> Nothing

parseQuoted :: ByteString -> Maybe ByteString
parseQuoted txt = do
  ('"', rest) <- C.uncons txt
  let (path, suffix) = C.break (== '"') rest
  if C.null suffix then Nothing else Just path

-- | ASCII whitespace.
--
-- Deliberately not 'Data.Char.isSpace': applied to a byte, that would
-- classify 0xA0 (Latin-1 NBSP, and a perfectly ordinary UTF-8
-- continuation byte) as whitespace and split a multi-byte character in
-- half. For the same reason this module avoids 'C.words' and 'C.strip'.
isSpaceChar :: Char -> Bool
isSpaceChar c =
  c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'

-- | Drop leading ASCII whitespace.
stripStart :: ByteString -> ByteString
stripStart = C.dropWhile isSpaceChar

-- | Drop leading and trailing ASCII whitespace.
strip :: ByteString -> ByteString
strip = C.dropWhile isSpaceChar . C.dropWhileEnd isSpaceChar

-- | First character of an identifier.
--
-- Any byte >= 0x80 qualifies, so a non-ASCII identifier is scanned as one
-- token regardless of the source encoding, and a byte that decodes to
-- nothing at all is simply part of whatever token contains it.
isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAsciiAlpha c || c >= '\x80'

-- | Subsequent characters of an identifier. See 'isIdentStart'.
isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || isAsciiAlpha c || isDigit c || c >= '\x80'

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

isOpChar :: Char -> Bool
isOpChar c =
  c == '+'
    || c == '-'
    || c == '*'
    || c == '/'
    || c == '%'
    || c == '&'
    || c == '|'
    || c == '!'
    || c == '='
    || c == '<'
    || c == '>'
