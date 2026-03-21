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
      gapSize <- numericLiteral
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
  entries <- parseFile parseConstEntries path
  pure [formatIndexRow bitIndex eventName | (bitIndex, eventName) <- entries]

-- | Extract toggleable object flag indices from constants/toggle_constants.asm.
extractToggleFlags :: FilePath -> IO [[Text]]
extractToggleFlags path = do
  entries <- parseFile parseConstEntries path
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
