{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Types
  ( LexTokenKind (..),
    pattern TkVarRole,
    pattern TkVarFamily,
    pattern TkVarAs,
    pattern TkVarHiding,
    pattern TkVarQualified,
    pattern TkVarSafe,
    TokenOrigin (..),
    LexToken (..),
    LexerEnv (..),
    hasExt,
    LexerState (..),
    LayoutContext (..),
    LayoutFrame (..),
    LayoutSemicolons (..),
    LayoutIndentPolicy (..),
    LayoutBaseline (..),
    ImplicitLayoutSpec (..),
    PendingLayout (..),
    ModuleLayoutMode (..),
    LayoutState (..),
    DirectiveUpdate (..),
    HashLineTrivia (..),
    mkLexerEnv,
    mkInitialLexerState,
    mkInitialLayoutState,
    mkToken,
    mkErrorToken,
    mkSpan,
    advanceChars,
    advanceN,
    consumeWhile,
    skipWhitespace,
    utf8CharWidth,
    tokenStartCol,
    virtualSymbolToken,
    isSymbolicOpChar,
    isUnicodeSymbol,
    isUnicodeSymbolCategory,
  )
where

import Aihc.Parser.Syntax
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (NFData)
import Data.Char (GeneralCategory (..), generalCategory, isAscii, isSpace, ord)
import Data.Data (Data)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe qualified as TU
import GHC.Generics (Generic)

data LexTokenKind
  = -- Keywords (reserved identifiers per Haskell Report Section 2.4)
    TkKeywordCase
  | TkKeywordClass
  | TkKeywordData
  | TkKeywordDefault
  | TkKeywordDeriving
  | TkKeywordDo
  | TkKeywordElse
  | TkKeywordForall
  | TkKeywordForeign
  | TkKeywordIf
  | TkKeywordImport
  | TkKeywordIn
  | TkKeywordInfix
  | TkKeywordInfixl
  | TkKeywordInfixr
  | TkKeywordInstance
  | TkKeywordLet
  | TkKeywordModule
  | TkKeywordNewtype
  | TkKeywordOf
  | TkKeywordThen
  | TkKeywordType
  | TkKeywordWhere
  | TkKeywordUnderscore
  | -- Extension-conditional keywords
    TkKeywordProc
  | TkKeywordRec
  | TkKeywordMdo
  | TkKeywordPattern
  | TkKeywordBy
  | TkKeywordUsing
  | -- QualifiedDo tokens (extension-conditional)

    -- | @M.do@ — carries the module qualifier
    TkQualifiedDo Text
  | -- | @M.mdo@ — carries the module qualifier
    TkQualifiedMdo Text
  | -- Reserved operators (per Haskell Report Section 2.4)
    TkReservedDotDot
  | TkReservedColon
  | TkReservedDoubleColon
  | TkReservedEquals
  | TkReservedBackslash
  | TkReservedPipe
  | TkReservedLeftArrow
  | TkReservedRightArrow
  | TkReservedAt
  | TkReservedDoubleArrow
  | -- LinearTypes multiplicity prefix (e.g. @%1@, @%m@, @%'Many@)
    TkPrefixPercent
  | -- LinearTypes arrow operator
    TkLinearArrow
  | -- Arrow notation reserved operators (Arrows extension)
    TkArrowTail
  | TkArrowTailReverse
  | TkDoubleArrowTail
  | TkDoubleArrowTailReverse
  | TkBananaOpen
  | TkBananaClose
  | -- Identifiers (per Haskell Report Section 2.4)
    TkVarId Text
  | TkConId Text
  | TkQVarId Text Text
  | TkQConId Text Text
  | TkImplicitParam Text
  | -- Operators (per Haskell Report Section 2.4)
    TkVarSym Text
  | TkConSym Text
  | TkQVarSym Text Text
  | TkQConSym Text Text
  | -- Literals
    TkInteger Integer NumericType
  | TkFloat Rational FloatType
  | TkChar Char
  | TkCharHash Char Text
  | TkString Text
  | TkStringHash Text Text
  | TkOverloadedLabel Text Text
  | -- Special characters (per Haskell Report Section 2.2)
    TkSpecialLParen
  | TkSpecialRParen
  | TkSpecialUnboxedLParen
  | TkSpecialUnboxedRParen
  | TkSpecialComma
  | TkSpecialSemicolon
  | TkSpecialLBracket
  | TkSpecialRBracket
  | TkSpecialBacktick
  | TkSpecialLBrace
  | TkSpecialRBrace
  | -- LexicalNegation support
    TkMinusOperator
  | TkPrefixMinus
  | -- Whitespace-sensitive operator support (GHC proposal 0229)
    TkPrefixBang
  | TkPrefixTilde
  | -- OverloadedRecordDot: '.' immediately after an expression, before a lowercase ident
    TkRecordDot
  | -- TypeApplications support
    TkTypeApp
  | -- Pragmas
    TkPragma Pragma
  | -- | The opening of a pragma whose body is lexed as ordinary tokens,
    -- such as @{-# RULES@. The text is the pragma keyword in upper case.
    TkPragmaOpen Text
  | -- | The @#-}@ that closes a pragma opened by 'TkPragmaOpen'.
    TkPragmaClose
  | -- TemplateHaskellQuotes bracket tokens
    TkTHExpQuoteOpen
  | TkTHExpQuoteClose
  | TkTHTypedQuoteOpen
  | TkTHTypedQuoteClose
  | TkTHDeclQuoteOpen
  | TkTHTypeQuoteOpen
  | TkTHPatQuoteOpen
  | TkTHQuoteTick
  | TkTHTypeQuoteTick
  | -- TemplateHaskell splice tokens (prefix $ and $$)
    TkTHSplice
  | TkTHTypedSplice
  | -- Other
    TkQuasiQuote Text Text
  | TkLineComment
  | TkBlockComment
  | TkError Text
  | TkEOF
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

pattern TkVarRole :: LexTokenKind
pattern TkVarRole = TkVarId "role"

pattern TkVarFamily :: LexTokenKind
pattern TkVarFamily = TkVarId "family"

pattern TkVarAs :: LexTokenKind
pattern TkVarAs = TkVarId "as"

pattern TkVarHiding :: LexTokenKind
pattern TkVarHiding = TkVarId "hiding"

pattern TkVarQualified :: LexTokenKind
pattern TkVarQualified = TkVarId "qualified"

pattern TkVarSafe :: LexTokenKind
pattern TkVarSafe = TkVarId "safe"

data TokenOrigin
  = FromSource
  | InsertedLayout
  deriving (Eq, Ord, Show, Read, Generic, NFData)

data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan,
    lexTokenOrigin :: !TokenOrigin,
    lexTokenAtLineStart :: !Bool
  }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype LexerEnv = LexerEnv
  { lexerExtensions :: ExtensionSet
  }
  deriving (Eq, Show)

hasExt :: Extension -> LexerEnv -> Bool
hasExt ext env = memberExtension ext (lexerExtensions env)
{-# INLINE hasExt #-}

data LexerState = LexerState
  { lexerInput :: !Text,
    lexerLogicalSourceName :: !Text,
    lexerLine :: !Int,
    lexerCol :: !Int,
    lexerByteOffset :: !Int,
    lexerAtLineStart :: !Bool,
    lexerPrevTokenKind :: !(Maybe LexTokenKind),
    lexerHadTrivia :: !Bool,
    -- | Whether the lexer is inside a pragma opened by 'TkPragmaOpen', so
    -- that the next @#-}@ closes it.
    lexerInPragma :: !Bool
  }
  deriving (Eq, Show)

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !LayoutFrame
  | LayoutDelimiter
  deriving (Eq, Show)

data LayoutFrame = LayoutFrame
  { layoutFrameIndent :: !Int,
    layoutFrameSemicolons :: !LayoutSemicolons,
    layoutFrameChildBaseline :: !LayoutBaseline,
    -- do-block opened directly by a preceding 'then'/'else'; tracks nested
    -- classic ifs inside the block while the lexer still approximates this
    -- parse-error close.
    layoutFrameThenElseDepth :: !(Maybe Int)
  }
  deriving (Eq, Show)

data LayoutSemicolons
  = LayoutEmitSemicolons
  | LayoutSuppressSemicolons
  deriving (Eq, Show)

data LayoutIndentPolicy
  = LayoutStrictIndent
  | LayoutAllowNondecreasingIndent
  deriving (Eq, Show)

data LayoutBaseline
  = LayoutCurrentIndent
  | LayoutColumnZero
  deriving (Eq, Show)

data ImplicitLayoutSpec = ImplicitLayoutSpec
  { implicitLayoutSemicolons :: !LayoutSemicolons,
    implicitLayoutIndentPolicy :: !LayoutIndentPolicy,
    implicitLayoutBaseline :: !LayoutBaseline,
    implicitLayoutChildBaseline :: !LayoutBaseline,
    implicitLayoutThenElseDepth :: !(Maybe Int),
    implicitLayoutFlushEmptyAtEOF :: !Bool
  }
  deriving (Eq, Show)

data PendingLayout
  = PendingImplicitLayout !ImplicitLayoutSpec
  | PendingMaybeMultiWayIf
  | PendingMaybeLambdaCases
  deriving (Eq, Show)

data ModuleLayoutMode
  = ModuleLayoutOff
  | ModuleLayoutSeekStart
  | ModuleLayoutAwaitWhere
  | ModuleLayoutAwaitBody
  | ModuleLayoutDone
  deriving (Eq, Show)

data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !(Maybe PendingLayout),
    layoutPrevTokenKind :: !(Maybe LexTokenKind),
    layoutModuleMode :: !ModuleLayoutMode,
    layoutPrevTokenEndSpan :: !(Maybe SourceSpan),
    layoutBuffer :: [LexToken],
    layoutNondecreasingIndent :: !Bool,
    layoutLambdaCase :: !Bool
  }
  deriving (Eq, Show)

data DirectiveUpdate = DirectiveUpdate
  { directiveLine :: !(Maybe Int),
    directiveCol :: !(Maybe Int),
    directiveSourceName :: !(Maybe Text)
  }
  deriving (Eq, Show)

data HashLineTrivia
  = HashLineDirective !DirectiveUpdate
  | HashLineShebang
  | HashLineMalformed
  deriving (Eq, Show)

mkLexerEnv :: [Extension] -> LexerEnv
mkLexerEnv exts = LexerEnv {lexerExtensions = mkExtensionSet exts}

mkInitialLexerState :: FilePath -> [Extension] -> Text -> (LexerEnv, LexerState)
mkInitialLexerState sourceName exts input =
  ( mkLexerEnv exts,
    LexerState
      { lexerInput = input,
        lexerLogicalSourceName = T.pack sourceName,
        lexerLine = 1,
        lexerCol = 1,
        lexerByteOffset = 0,
        lexerAtLineStart = True,
        lexerPrevTokenKind = Nothing,
        lexerHadTrivia = True,
        lexerInPragma = False
      }
  )

mkInitialLayoutState :: Bool -> [Extension] -> LayoutState
mkInitialLayoutState enableModuleLayout exts =
  LayoutState
    { layoutContexts = [],
      layoutPendingLayout = Nothing,
      layoutPrevTokenKind = Nothing,
      layoutModuleMode =
        if enableModuleLayout
          then ModuleLayoutSeekStart
          else ModuleLayoutOff,
      layoutPrevTokenEndSpan = Nothing,
      layoutBuffer = [],
      layoutNondecreasingIndent = Syntax.NondecreasingIndentation `elem` exts,
      layoutLambdaCase = Syntax.LambdaCase `elem` exts
    }

mkToken :: LexerState -> LexerState -> Text -> LexTokenKind -> LexToken
mkToken start end tokTxt kind =
  LexToken
    { lexTokenKind = kind,
      lexTokenText = tokTxt,
      lexTokenSpan = mkSpan start end,
      lexTokenOrigin = FromSource,
      lexTokenAtLineStart = lexerAtLineStart start
    }

mkErrorToken :: LexerState -> LexerState -> Text -> Text -> LexToken
mkErrorToken start end rawTxt msg = mkToken start end rawTxt (TkError msg)

mkSpan :: LexerState -> LexerState -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanSourceName = lexerLogicalSourceName start,
      sourceSpanStartLine = lexerLine start,
      sourceSpanStartCol = lexerCol start,
      sourceSpanEndLine = lexerLine end,
      sourceSpanEndCol = lexerCol end,
      sourceSpanStartOffset = lexerByteOffset start,
      sourceSpanEndOffset = lexerByteOffset end
    }

-- | Consume a prefix of the input, updating the source position.
--
-- Every character advances the byte offset by its own UTF-8 width, so the new
-- offset is just the old one plus the byte length of @consumed@; only the
-- line/column/line-start fields need a scan.  That scan walks byte indices
-- with 'TU.iter' so the UTF-8 width of each character comes from the iterator
-- instead of being recomputed from the character.
advanceChars :: Text -> LexerState -> LexerState
advanceChars consumed st =
  let !nbytes = TU.lengthWord8 consumed
      go !i !line !col !atLineStart
        | i >= nbytes = (line, col, atLineStart)
        | otherwise =
            let TU.Iter ch d = TU.iter consumed i
             in case ch of
                  '\n' -> go (i + d) (line + 1) 1 True
                  '\t' ->
                    let nextTabStop = 8 - ((col - 1) `mod` 8)
                     in go (i + d) line (col + nextTabStop) atLineStart
                  _
                    -- Printable ASCII is the overwhelmingly common case and
                    -- is never a space character, so it is settled before the
                    -- 'isSpace' test.  Space itself (and any other space
                    -- character) falls through to the guard below, which
                    -- leaves 'atLineStart' alone exactly as before.
                    | ch > ' ' && isAscii ch -> go (i + d) line (col + 1) False
                    | isSpace ch -> go (i + d) line (col + 1) atLineStart
                    | otherwise -> go (i + d) line (col + 1) False
      (!finalLine, !finalCol, !finalAtLineStart) =
        go 0 (lexerLine st) (lexerCol st) (lexerAtLineStart st)
   in st
        { -- The consumed text is a prefix of the input, so dropping its UTF-8
          -- byte length avoids a second scan of the characters.
          lexerInput = TU.dropWord8 nbytes (lexerInput st),
          lexerLine = finalLine,
          lexerCol = finalCol,
          lexerByteOffset = lexerByteOffset st + nbytes,
          lexerAtLineStart = finalAtLineStart
        }

advanceN :: Int -> LexerState -> LexerState
advanceN n st = advanceChars (T.take n (lexerInput st)) st

consumeWhile :: (Char -> Bool) -> LexerState -> LexerState
consumeWhile f st =
  let consumed = T.takeWhile f (lexerInput st)
   in advanceChars consumed st

-- | Skip leading Haskell whitespace in one pass and record that trivia was
-- seen. Equivalent to @markHadTrivia (consumeWhile isHaskellWhitespace)@ but
-- without the intermediate slice and the second lexer-state copy.
skipWhitespace :: LexerState -> LexerState
skipWhitespace st =
  let input = lexerInput st
      !len = TU.lengthWord8 input
      go !i !line !col !byteOff !atLineStart
        | i >= len = finish i line col byteOff atLineStart
        | otherwise =
            let TU.Iter ch d = TU.iter input i
             in case ch of
                  '\n' -> go (i + d) (line + 1) 1 (byteOff + 1) True
                  '\t' ->
                    let nextTabStop = 8 - ((col - 1) `mod` 8)
                     in go (i + d) line (col + nextTabStop) (byteOff + 1) atLineStart
                  ' ' -> go (i + d) line (col + 1) (byteOff + 1) atLineStart
                  _
                    | isSpace ch -> go (i + d) line (col + 1) (byteOff + d) atLineStart
                    | otherwise -> finish i line col byteOff atLineStart
      finish i line col byteOff atLineStart =
        st
          { lexerInput = TU.dropWord8 i input,
            lexerLine = line,
            lexerCol = col,
            lexerByteOffset = byteOff,
            lexerAtLineStart = atLineStart,
            lexerHadTrivia = True
          }
   in go 0 (lexerLine st) (lexerCol st) (lexerByteOffset st) (lexerAtLineStart st)

tokenStartCol :: LexToken -> Int
tokenStartCol tok = sourceSpanStartCol (lexTokenSpan tok)

virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = case sym of
        "{" -> TkSpecialLBrace
        "}" -> TkSpecialRBrace
        ";" -> TkSpecialSemicolon
        _ -> error ("virtualSymbolToken: unknown symbol: " ++ show sym),
      lexTokenText = sym,
      lexTokenSpan = span',
      lexTokenOrigin = InsertedLayout,
      lexTokenAtLineStart = False
    }

utf8CharWidth :: Char -> Int
utf8CharWidth ch =
  case ord ch of
    code
      | code <= 0x7F -> 1
      | code <= 0x7FF -> 2
      | code <= 0xFFFF -> 3
      | otherwise -> 4

-- | Characters that may appear in a symbolic operator.
--
-- The ASCII branch lists exactly @:!#$%&*+./\<=>?\@\\^|-~@; every ASCII
-- character that 'isUnicodeSymbol' would accept (@+ \< = > | ~ $@) is already
-- in that list, so the split loses nothing.  It is written as a @case@ rather
-- than @elem@ over a string because this predicate runs on nearly every lexed
-- character and @elem@ would walk a 21-element list each time.
isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c
  | isAscii c =
      case c of
        ':' -> True
        '!' -> True
        '#' -> True
        '$' -> True
        '%' -> True
        '&' -> True
        '*' -> True
        '+' -> True
        '.' -> True
        '/' -> True
        '<' -> True
        '=' -> True
        '>' -> True
        '?' -> True
        '@' -> True
        '\\' -> True
        '^' -> True
        '|' -> True
        '-' -> True
        '~' -> True
        _ -> False
  | otherwise = isUnicodeSymbol c

isUnicodeSymbol :: Char -> Bool
isUnicodeSymbol c =
  isUnicodeSymbolCategory c
    || c == '∷'
    || c == '⇒'
    || c == '→'
    || c == '←'
    || c == '∀'
    || c == '⤙'
    || c == '⤚'
    || c == '⤛'
    || c == '⤜'
    || c == '⦇'
    || c == '⦈'
    || c == '⟦'
    || c == '⟧'
    || c == '⊸'

isUnicodeSymbolCategory :: Char -> Bool
isUnicodeSymbolCategory c =
  case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> not (isAscii c)
    OtherSymbol -> True
    ConnectorPunctuation -> not (isAscii c)
    DashPunctuation -> not (isAscii c)
    OtherPunctuation -> not (isAscii c)
    _ -> False
