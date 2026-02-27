{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec parsers for pret (pokered/pokecrystal) RGBASM source files.
--
-- pret uses a handful of recurring patterns across all its data files:
-- const blocks for enumerations, db/dn/dw for data, and custom macros
-- like tmhm and move. This module provides composable parsers for the
-- shared patterns; file-specific parsers live in their Extract.* modules.

module Extract.ASM
  ( -- * Parser type
    Parser

    -- * Running parsers
  , parseFile

    -- * Low-level pieces
  , lexeme
  , symbol
  , keyword
  , identifier
  , integer
  , hexInteger
  , horizontalSpace
  , endOfLine
  , lineComment
  , blankLine
  , skipJunk

    -- * Const blocks
  , ConstEntry(..)
  , parseConstBlock

    -- * TM/HM/Tutor macros
  , TMHM(..)
  , parseTMHMBlock

    -- * Common data line parsers
  , dbArgs
  , commaSeparated
  ) where

import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (hspace, eol)  -- we define our own horizontalSpace and endOfLine
import qualified Text.Megaparsec.Char.Lexer as L


-- ── Parser type ────────────────────────────────────────────────

-- | Our parser operates on Text with no custom error type.
type Parser = Parsec Void Text


-- ── Running parsers ────────────────────────────────────────────

-- | Parse a file from disk. Crashes on parse failure — these are
-- pret sources under our control, not user input.
parseFile :: Parser a -> FilePath -> IO a
parseFile p path = do
  content <- T.readFile path
  case parse p path content of
    Left err -> error (errorBundlePretty err)
    Right a  -> pure a


-- ── Low-level pieces ───────────────────────────────────────────

-- | Consume horizontal whitespace (spaces and tabs, not newlines).
horizontalSpace :: Parser ()
horizontalSpace = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')

-- | Run a parser, then consume trailing horizontal whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme horizontalSpace

-- | Match a specific string, then consume trailing horizontal whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol horizontalSpace

-- | Match a keyword — an exact string NOT followed by more identifier
-- characters.  This prevents "add_tm" from matching the prefix of
-- "add_tmnum", or "const" from matching "const_def".
keyword :: Text -> Parser Text
keyword w = lexeme $ try (chunk w <* notFollowedBy (satisfy isIdentifierChar))

-- | An ASM identifier: a letter or underscore, then any mix of
-- letters, digits, and underscores.
identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\c -> c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
  rest <- takeWhileP (Just "identifier") isIdentifierChar
  pure (T.cons c rest)

-- | Character that can appear in an ASM identifier.
isIdentifierChar :: Char -> Bool
isIdentifierChar c = c == '_' || (c >= 'A' && c <= 'Z')
                         || (c >= 'a' && c <= 'z')
                         || (c >= '0' && c <= '9')

-- | A decimal integer, possibly negative.
integer :: Parser Int
integer = lexeme L.decimal

-- | A hex integer prefixed with $ (pret convention) or 0x.
hexInteger :: Parser Int
hexInteger = lexeme $ do
  _ <- single '$' <|> (chunk "0x" *> pure 'x')
  L.hexadecimal :: Parser Int

-- | A number that may be decimal or $hex.
number :: Parser Int
number = try hexInteger <|> integer

-- | Skip a ; comment through end of line.
lineComment :: Parser ()
lineComment = L.skipLineComment ";"

-- | Consume a newline, or do nothing at end of input.
-- Most pret files end with a newline, but we don't want to fail
-- on a final line that lacks one.
endOfLine :: Parser ()
endOfLine = void newline <|> void eof

-- | Skip a blank or comment-only line.
blankLine :: Parser ()
blankLine = horizontalSpace *> optional lineComment *> endOfLine

-- | Skip any number of blank lines, comment-only lines, and lines
-- we don't care about (INCLUDE, SECTION, MACRO/ENDM, etc.).
-- Stops when the next line would be consumed by a real parser.
skipJunk :: Parser ()
skipJunk = skipMany $ try junkLine
  where
    junkLine = do
      horizontalSpace
      -- A line is junk if it's blank, a comment, or starts with a
      -- keyword we want to skip
      choice
        [ void lineComment *> void newline
        , void newline  -- blank line
        , try $ do
            w <- lookAhead (takeWhile1P Nothing (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= ';'))
            if w `elem` junkKeywords
              then void (takeWhileP Nothing (/= '\n')) *> void newline
              else empty
        , try $ do
            -- Lines starting with a label (word followed by ::) or
            -- DEF ... EQU, INCLUDE, SECTION, etc.
            _ <- lookAhead (satisfy (\c -> c >= 'A' && c <= 'Z'))
            void (takeWhileP Nothing (/= '\n')) *> void newline
        ]
    junkKeywords :: [Text]
    junkKeywords =
      [ "INCLUDE", "SECTION", "MACRO", "ENDM", "ASSERT"
      , "assert_list_length", "assert_table_length"
      , "table_width", "list_start"
      , "INCBIN", "dw", "db"
      ]


-- ── Const blocks ───────────────────────────────────────────────

-- | One entry from a const block: the name and its resolved value.
data ConstEntry = ConstEntry
  { constName  :: !Text
  , constValue :: !Int
  } deriving (Show)

-- | Parse a file that contains const_def/const/const_next/const_skip
-- directives. Extracts all `const NAME` definitions into a Map.
--
-- Skips everything it doesn't understand (comments, DEF lines,
-- MACRO blocks, etc.) — only tracks the counter state machine:
--
--   const_def [start]   — reset counter (default 0)
--   const NAME          — define NAME = counter, counter += 1
--   const_skip          — counter += 1, no definition
--   const_next VALUE    — set counter to VALUE
parseConstBlock :: Parser (Map Text Int)
parseConstBlock = do
  entries <- constLines 0
  pure $ Map.fromList [(constName e, constValue e) | e <- entries]
  where
    constLines :: Int -> Parser [ConstEntry]
    constLines counter = do
      done <- isAtEnd
      if done
        then pure []
        else do
          result <- constLine counter
          case result of
            ConstDef n      -> constLines n
            ConstNext n     -> constLines n
            ConstSkip       -> constLines (counter + 1)
            ConstFound e    -> (e :) <$> constLines (counter + 1)
            ConstIgnored    -> constLines counter

    -- Parse one line, returning what happened to the counter.
    constLine :: Int -> Parser ConstLineResult
    constLine counter = do
      horizontalSpace
      choice
        [ try $ pConstDef
        , try $ pConstNext
        , try $ pConstSkip
        , try $ pConst counter
        , ConstIgnored <$ (takeWhileP Nothing (/= '\n') *> endOfLine)
        ]

    pConstDef :: Parser ConstLineResult
    pConstDef = do
      _ <- keyword "const_def"
      n <- option 0 number
      _ <- takeWhileP Nothing (/= '\n')
      endOfLine
      pure (ConstDef n)

    pConstNext :: Parser ConstLineResult
    pConstNext = do
      _ <- keyword "const_next"
      n <- number
      _ <- takeWhileP Nothing (/= '\n')
      endOfLine
      pure (ConstNext n)

    pConstSkip :: Parser ConstLineResult
    pConstSkip = do
      _ <- keyword "const_skip"
      _ <- takeWhileP Nothing (/= '\n')
      endOfLine
      pure ConstSkip

    pConst :: Int -> Parser ConstLineResult
    pConst counter = do
      _ <- keyword "const"
      name <- identifier
      _ <- takeWhileP Nothing (/= '\n')
      endOfLine
      pure (ConstFound (ConstEntry name counter))

    isAtEnd :: Parser Bool
    isAtEnd = option False (True <$ eof)

-- Internal: what a const line did to the state.
data ConstLineResult
  = ConstDef  !Int
  | ConstNext !Int
  | ConstSkip
  | ConstFound !ConstEntry
  | ConstIgnored


-- ── TM/HM/Tutor macros ────────────────────────────────────────

-- | Parsed TM/HM/Tutor data from item_constants.asm.
data TMHM = TMHM
  { tmMoves    :: ![Text]  -- ^ TM move names in order (TM01, TM02, ...)
  , hmMoves    :: ![Text]  -- ^ HM move names in order (HM01, HM02, ...)
  , tutorMoves :: ![Text]  -- ^ Tutor move names in order (Gen 2 only)
  } deriving (Show)

-- | Parse item_constants.asm, extracting add_tm/add_hm/add_mt lines.
-- Ignores everything else (item const definitions, DEF lines, etc.).
parseTMHMBlock :: Parser TMHM
parseTMHMBlock = do
  (tms, hms, mts) <- go [] [] []
  pure TMHM
    { tmMoves    = reverse tms
    , hmMoves    = reverse hms
    , tutorMoves = reverse mts
    }
  where
    go tms hms mts = do
      done <- option False (True <$ eof)
      if done
        then pure (tms, hms, mts)
        else do
          horizontalSpace
          choice
            [ try (pAddTm  >>= \name -> go (name : tms) hms mts)
            , try (pAddHm  >>= \name -> go tms (name : hms) mts)
            , try (pAddMt  >>= \name -> go tms hms (name : mts))
            , skipLine >> go tms hms mts
            ]

    pAddTm = keyword "add_tm" *> identifier <* restOfLine
    pAddHm = keyword "add_hm" *> identifier <* restOfLine
    pAddMt = keyword "add_mt" *> identifier <* restOfLine

    skipLine = takeWhileP Nothing (/= '\n') *> endOfLine
    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine


-- ── Common data line parsers ───────────────────────────────────

-- | Parse a `db` line's arguments (everything after `db`).
-- Returns the raw text of each comma-separated argument, stripped.
dbArgs :: Parser [Text]
dbArgs = do
  horizontalSpace
  _ <- keyword "db"
  commaSeparated

-- | Parse comma-separated arguments until end of line or comment.
-- Each argument is stripped of whitespace.
commaSeparated :: Parser [Text]
commaSeparated = do
  args <- sepBy1 arg (lexeme (single ','))
  _ <- takeWhileP Nothing (/= '\n')
  endOfLine
  pure args
  where
    arg = T.strip <$> takeWhile1P (Just "argument") (\c -> c /= ',' && c /= ';' && c /= '\n')
