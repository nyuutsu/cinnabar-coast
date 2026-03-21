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
  , numericLiteral
  , horizontalSpace
  , endOfLine
  , lineComment
  , blankLine
  , skipJunk

    -- * Const directives
  , ConstDirective(..)
  , parseConstDirectiveLine
  , resolveConstDirectives

    -- * Const block parsers
  , parseConstEntries
  , parseConstBlock
  , parseConstEntriesUntil

    -- * TM/HM/Tutor macros
  , TMHM(..)
  , parseTMHMBlock

    -- * Line scanning
  , restOfLine
  , scanLines

    -- * Common data line parsers
  , dbArgs
  , commaSeparated
  ) where

import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Text.Megaparsec hiding (parseError)
import Text.Megaparsec.Char hiding (hspace, eol, char)  -- we define our own horizontalSpace and endOfLine
import qualified Text.Megaparsec.Char.Lexer as L


-- ── Parser type ────────────────────────────────────────────────

-- | Our parser operates on Text with no custom error type.
type Parser = Parsec Void Text


-- ── Running parsers ────────────────────────────────────────────

-- | Parse a file from disk. Crashes on parse failure — these are
-- pret sources under our control, not user input.
parseFile :: Parser a -> FilePath -> IO a
parseFile parser path = do
  content <- TextIO.readFile path
  case parse parser path content of
    Left parseError -> error (errorBundlePretty parseError)
    Right result    -> pure result


-- ── Low-level pieces ───────────────────────────────────────────

-- | Consume horizontal whitespace (spaces and tabs, not newlines).
horizontalSpace :: Parser ()
horizontalSpace = void $ takeWhileP Nothing (\char -> char == ' ' || char == '\t')

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
keyword word = lexeme $ try (chunk word <* notFollowedBy (satisfy isIdentifierChar))

-- | An ASM identifier: a letter or underscore, then any mix of
-- letters, digits, and underscores.
identifier :: Parser Text
identifier = lexeme $ do
  firstChar <- satisfy (\char -> char == '_' || (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z'))
  rest <- takeWhileP (Just "identifier") isIdentifierChar
  pure (Text.cons firstChar rest)

-- | Character that can appear in an ASM identifier.
isIdentifierChar :: Char -> Bool
isIdentifierChar char = char == '_' || (char >= 'A' && char <= 'Z')
                         || (char >= 'a' && char <= 'z')
                         || (char >= '0' && char <= '9')

-- | A decimal integer, possibly negative.
integer :: Parser Int
integer = lexeme L.decimal

-- | A hex integer prefixed with $ (pret convention) or 0x.
hexInteger :: Parser Int
hexInteger = lexeme $ do
  _ <- single '$' <|> (chunk "0x" *> pure 'x')
  L.hexadecimal :: Parser Int

-- | A number that may be decimal or $hex.
numericLiteral :: Parser Int
numericLiteral = try hexInteger <|> integer

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
            leadingWord <- lookAhead (takeWhile1P Nothing (\char -> char /= ' ' && char /= '\t' && char /= '\n' && char /= ';'))
            if leadingWord `elem` junkKeywords
              then void (takeWhileP Nothing (/= '\n')) *> void newline
              else empty
        , try $ do
            -- Lines starting with a label (word followed by ::) or
            -- DEF ... EQU, INCLUDE, SECTION, etc.
            _ <- lookAhead (satisfy (\char -> char >= 'A' && char <= 'Z'))
            void (takeWhileP Nothing (/= '\n')) *> void newline
        ]
    junkKeywords :: [Text]
    junkKeywords =
      [ "INCLUDE", "SECTION", "MACRO", "ENDM", "ASSERT"
      , "assert_list_length", "assert_table_length"
      , "table_width", "list_start"
      , "INCBIN", "dw", "db"
      ]


-- ── Const directives ─────────────────────────────────────────

-- | One directive from a const-based ASM file.
data ConstDirective
  = ConstReset !Int       -- ^ @const_def [start]@: reset counter (default 0)
  | ConstJump !Int        -- ^ @const_next VALUE@: set counter to value
  | ConstSkip !Int        -- ^ @const_skip [N]@: advance counter by N (default 1)
  | ConstNamed !Text      -- ^ @const NAME@: define NAME at current counter
  deriving (Show)

-- | Try to parse one line as a const directive. Returns 'Just' for
-- recognized directives, 'Nothing' for other lines. Consumes
-- leading whitespace and everything through end of line.
parseConstDirectiveLine :: Parser (Maybe ConstDirective)
parseConstDirectiveLine = do
  horizontalSpace
  choice
    [ Just <$> try constDirective
    , Nothing <$ restOfLine
    ]

-- | Walk a list of const directives, tracking the running counter.
-- Produces (index, name) for each 'ConstNamed'.
resolveConstDirectives :: [ConstDirective] -> [(Int, Text)]
resolveConstDirectives = resolveFrom 0
  where
    resolveFrom _counter [] = []
    resolveFrom counter (directive : rest) = case directive of
      ConstReset startValue -> resolveFrom startValue rest
      ConstJump nextValue   -> resolveFrom nextValue rest
      ConstSkip skipAmount  -> resolveFrom (counter + skipAmount) rest
      ConstNamed name       -> (counter, name) : resolveFrom (counter + 1) rest

-- Internal: parse a const directive after leading whitespace.
-- Consumes through end of line on success.
constDirective :: Parser ConstDirective
constDirective = choice
  [ try parseReset
  , try parseJump
  , try parseSkipDirective
  , parseNamed
  ]
  where
    parseReset = do
      _ <- keyword "const_def"
      startValue <- option 0 numericLiteral
      restOfLine
      pure (ConstReset startValue)

    parseJump = do
      _ <- keyword "const_next"
      baseValue <- numericLiteral
      -- Handle arithmetic: const_next $F0 - 2, const_next $100 + 3
      finalValue <- option baseValue $ do
        horizontalSpace
        operator <- single '-' <|> single '+'
        horizontalSpace
        offset <- numericLiteral
        pure $ case operator of
          '-' -> baseValue - offset
          '+' -> baseValue + offset
          _   -> baseValue  -- unreachable, but total
      restOfLine
      pure (ConstJump finalValue)

    parseSkipDirective = do
      _ <- keyword "const_skip"
      skipAmount <- option 1 (try numericLiteral)
      restOfLine
      pure (ConstSkip skipAmount)

    parseNamed = do
      _ <- keyword "const"
      name <- identifier
      restOfLine
      pure (ConstNamed name)


-- ── Const block parsers ──────────────────────────────────────

-- | Parse const directives to EOF, resolving named entries to
-- their indices. Skips non-directive lines.
parseConstEntries :: Parser [(Int, Text)]
parseConstEntries = resolveConstDirectives <$> collectDirectives []
  where
    collectDirectives results = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse results)
        else do
          result <- parseConstDirectiveLine
          case result of
            Just directive -> collectDirectives (directive : results)
            Nothing        -> collectDirectives results

-- | Parse const directives to EOF, collecting named entries into
-- a Map from name to index.
parseConstBlock :: Parser (Map Text Int)
parseConstBlock = do
  entries <- parseConstEntries
  pure $ Map.fromList [(name, idx) | (idx, name) <- entries]

-- | Parse const directives until a stop parser matches at the
-- start of a line (after leading whitespace). The stop line is
-- consumed but not included in results.
parseConstEntriesUntil :: Parser stop -> Parser [(Int, Text)]
parseConstEntriesUntil stopParser = resolveConstDirectives <$> collectDirectives []
  where
    collectDirectives results = do
      horizontalSpace
      stopped <- option False (True <$ try (lookAhead stopParser))
      if stopped
        then restOfLine *> pure (reverse results)
        else choice
          [ try (constDirective >>= \directive -> collectDirectives (directive : results))
          , restOfLine >> collectDirectives results
          ]


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
  (tms, hms, tutors) <- collectEntries [] [] []
  pure TMHM
    { tmMoves    = reverse tms
    , hmMoves    = reverse hms
    , tutorMoves = reverse tutors
    }
  where
    collectEntries tms hms tutors = do
      done <- option False (True <$ eof)
      if done
        then pure (tms, hms, tutors)
        else do
          horizontalSpace
          choice
            [ try (parseAddTm >>= \name -> collectEntries (name : tms) hms tutors)
            , try (parseAddHm >>= \name -> collectEntries tms (name : hms) tutors)
            , try (parseAddMt >>= \name -> collectEntries tms hms (name : tutors))
            , restOfLine >> collectEntries tms hms tutors
            ]

    parseAddTm = keyword "add_tm" *> identifier <* restOfLine
    parseAddHm = keyword "add_hm" *> identifier <* restOfLine
    parseAddMt = keyword "add_mt" *> identifier <* restOfLine


-- ── Line scanning ────────────────────────────────────────────

-- | Consume the rest of a line (everything up to newline) and the
-- newline itself, or succeed at end of input.
restOfLine :: Parser ()
restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

-- | Try a parser on each line of the input; collect successes, skip
-- failures.  Consumes leading horizontal whitespace before each
-- attempt.
--
-- The parser passed in must consume through end-of-line on success.
scanLines :: Parser a -> Parser [a]
scanLines lineParser = reverse <$> collectResults []
  where
    collectResults results = do
      done <- option False (True <$ eof)
      if done
        then pure results
        else do
          horizontalSpace
          choice
            [ try (lineParser >>= \result -> collectResults (result : results))
            , restOfLine >> collectResults results
            ]


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
  args <- sepBy1 singleArgument (lexeme (single ','))
  _ <- takeWhileP Nothing (/= '\n')
  endOfLine
  pure args
  where
    singleArgument = Text.strip <$> takeWhile1P (Just "argument") (\char -> char /= ',' && char /= ';' && char /= '\n')
