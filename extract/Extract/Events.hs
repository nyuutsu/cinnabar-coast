{-# LANGUAGE OverloadedStrings #-}

-- | Extract event flags, toggleable object flags, and map script
-- progress bytes from pokered ASM sources.
--
-- Event flags and toggle flags use the const_def/const/const_skip/
-- const_next pattern with a running index. Map scripts use a
-- different format: named labels with :: db for bytes and ds N
-- for unnamed gaps.

module Extract.Events
  ( -- * Event flags
    extractEventFlags
  , eventFlagsHeader

    -- * Toggle flags
  , extractToggleFlags
  , toggleFlagsHeader

    -- * Map scripts
  , extractMapScripts
  , mapScriptsHeader
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec

import Extract.ASM


-- ── Const directive parsing ──────────────────────────────────

-- | One directive from a const-based ASM file.
data ConstDirective
  = ResetCounter !Int       -- ^ const_def [start]: reset to start (default 0)
  | JumpCounter !Int        -- ^ const_next VALUE: set counter to value
  | SkipCounter !Int        -- ^ const_skip [N]: advance by N (default 1)
  | NamedConst !Text        -- ^ const NAME: named entry at current index
  deriving (Show)

-- | Parse a file of const directives, resolving each named const
-- to its bit/byte index. Skips lines that aren't const directives
-- (comments, MACRO blocks, toggle_consts_for, DEF lines, etc.).
parseConstDirectives :: Parser [(Int, Text)]
parseConstDirectives = resolveDirectives <$> scanLines parseDirective

-- | Parse one const directive from a line.
parseDirective :: Parser ConstDirective
parseDirective = choice
  [ try parseConstDef
  , try parseConstNext
  , try parseConstSkip
  , parseConstNamed
  ]
  where
    parseConstDef = do
      _ <- keyword "const_def"
      startValue <- option 0 number
      restOfLine
      pure (ResetCounter startValue)

    parseConstNext = do
      _ <- keyword "const_next"
      baseValue <- number
      -- Handle simple arithmetic: $F0 - 2, $100 + 3, etc.
      finalValue <- option baseValue $ do
        horizontalSpace
        op <- single '-' <|> single '+'
        horizontalSpace
        offset <- number
        pure $ case op of
          '-' -> baseValue - offset
          '+' -> baseValue + offset
          _   -> baseValue  -- unreachable, but total
      restOfLine
      pure (JumpCounter finalValue)

    parseConstSkip = do
      _ <- keyword "const_skip"
      -- Bare const_skip means skip 1; const_skip N means skip N
      skipAmount <- option 1 (try number)
      restOfLine
      pure (SkipCounter skipAmount)

    parseConstNamed = do
      _ <- keyword "const"
      name <- identifier
      restOfLine
      pure (NamedConst name)

-- | Walk a list of directives, tracking the running counter.
-- Emit (index, name) for each NamedConst.
resolveDirectives :: [ConstDirective] -> [(Int, Text)]
resolveDirectives = resolveFrom 0
  where
    resolveFrom _counter [] = []
    resolveFrom counter (directive : rest) = case directive of
      ResetCounter startValue -> resolveFrom startValue rest
      JumpCounter nextValue   -> resolveFrom nextValue rest
      SkipCounter skipAmount  -> resolveFrom (counter + skipAmount) rest
      NamedConst name         -> (counter, name) : resolveFrom (counter + 1) rest


-- ── Map script parsing ───────────────────────────────────────

-- | One entry in the wGameProgressFlags region of wram.asm.
data ProgressEntry
  = NamedByte !Text   -- ^ wFoo:: db — named byte, advances offset by 1
  | NamedLabel !Text  -- ^ wFoo:: (no db) — alias at current offset, no advance
  | UnnamedGap !Int   -- ^ ds N — unnamed gap, advances offset by N
  deriving (Show)

-- | Parse the wGameProgressFlags region of wram.asm: everything
-- between the wGameProgressFlags:: and wGameProgressFlagsEnd::
-- boundary labels. Tracks a running byte offset.
parseMapScripts :: Parser [(Int, Text)]
parseMapScripts = do
  skipToProgressFlags
  resolveProgressEntries <$> scanProgressEntries
  where
    -- Skip everything until we see wGameProgressFlags:: on its own line
    skipToProgressFlags = do
      done <- option False (True <$ eof)
      if done
        then fail "reached end of file without finding wGameProgressFlags::"
        else do
          found <- option False $ try $ do
            horizontalSpace
            _ <- chunk "wGameProgressFlags::"
            pure True
          if found
            then restOfLine  -- consume the rest of this line
            else restOfLine >> skipToProgressFlags

    -- Collect entries until wGameProgressFlagsEnd::
    scanProgressEntries :: Parser [ProgressEntry]
    scanProgressEntries = reverse <$> collectEntries []

    collectEntries results = do
      horizontalSpace
      reachedEnd <- option False $ try $ do
        _ <- chunk "wGameProgressFlagsEnd::"
        pure True
      if reachedEnd
        then pure results
        else do
          entry <- optional $ try parseProgressEntry
          case entry of
            Just progressEntry -> do
              restOfLine
              collectEntries (progressEntry : results)
            Nothing -> do
              restOfLine
              collectEntries results

    parseProgressEntry :: Parser ProgressEntry
    parseProgressEntry = choice
      [ try parseNamedByte
      , try parseNamedLabel
      , parseDs
      ]

    -- wFoo:: db
    parseNamedByte = do
      name <- parseLabelName
      _ <- keyword "db"
      pure (NamedByte name)

    -- wFoo:: (bare label, no db — alias for the next byte)
    parseNamedLabel = do
      name <- parseLabelName
      -- Must NOT be followed by db (that's parseNamedByte)
      notFollowedBy (keyword "db")
      pure (NamedLabel name)

    -- ds N
    parseDs = do
      _ <- keyword "ds"
      gapSize <- number
      pure (UnnamedGap gapSize)

    -- Parse a label name: identifier followed by ::
    parseLabelName = do
      name <- takeWhile1P (Just "label name") isLabelChar
      _ <- chunk "::"
      horizontalSpace
      pure name

    isLabelChar char =
      (char >= 'A' && char <= 'Z')
      || (char >= 'a' && char <= 'z')
      || (char >= '0' && char <= '9')
      || char == '_'

-- | Walk progress entries, tracking a running byte offset.
resolveProgressEntries :: [ProgressEntry] -> [(Int, Text)]
resolveProgressEntries = resolveFrom 0
  where
    resolveFrom _offset [] = []
    resolveFrom offset (entry : rest) = case entry of
      NamedByte name  -> (offset, name) : resolveFrom (offset + 1) rest
      NamedLabel name -> (offset, name) : resolveFrom offset rest
      UnnamedGap size -> resolveFrom (offset + size) rest


-- ── Exported extractors ──────────────────────────────────────

-- | Extract event flag bit indices from constants/event_constants.asm.
extractEventFlags :: FilePath -> IO [[Text]]
extractEventFlags path = do
  entries <- parseFile parseConstDirectives path
  pure [formatIndexRow bitIndex eventName | (bitIndex, eventName) <- entries]

-- | Extract toggleable object flag indices from constants/toggle_constants.asm.
extractToggleFlags :: FilePath -> IO [[Text]]
extractToggleFlags path = do
  entries <- parseFile parseConstDirectives path
  pure [formatIndexRow bitIndex toggleName | (bitIndex, toggleName) <- entries]

-- | Extract map script progress byte offsets from ram/wram.asm.
extractMapScripts :: FilePath -> IO [[Text]]
extractMapScripts path = do
  entries <- parseFile parseMapScripts path
  pure [formatIndexRow byteOffset scriptName | (byteOffset, scriptName) <- entries]

formatIndexRow :: Int -> Text -> [Text]
formatIndexRow index name = [Text.pack (show index), name]

eventFlagsHeader :: [Text]
eventFlagsHeader = ["bit_index", "event_name"]

toggleFlagsHeader :: [Text]
toggleFlagsHeader = ["bit_index", "toggle_name"]

mapScriptsHeader :: [Text]
mapScriptsHeader = ["byte_offset", "script_name"]
