{-# LANGUAGE OverloadedStrings #-}

-- | Structured errors for the game data loading layer.
--
-- The pure query layer (Legality, Stats) is total — errors live here,
-- at the boundary where CSVs become domain types. This module defines
-- the error type, a renderer, and a convenience crash function for
-- the current error-or-die loading style.

module Cinnabar.Error
  ( LoadError (..)
  , renderLoadError
  , loadOrDie
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Cinnabar.Types (ColumnName (..), DexNumber (..), EnumLabel (..), RowNumber (..))


-- ── Error type ────────────────────────────────────────────────────

-- | What can go wrong when loading game data from CSVs.
-- Each constructor carries enough context to produce a useful message
-- without the caller needing to thread file/row/column info manually.
data LoadError
  = EmptyCSV
      !FilePath
  | MissingColumn
      !FilePath
      !ColumnName       -- the column that's missing
      ![ColumnName]     -- columns that do exist
  | UnparseableInt
      !FilePath
      !RowNumber
      !ColumnName
      !Text           -- raw value
  | EmptyRequiredField
      !FilePath
      !RowNumber
      !ColumnName
  | RowTooShort
      !FilePath
      !RowNumber
      !Int              -- expected column count
      !Int              -- actual column count
  | UnknownEnum
      !FilePath
      !RowNumber
      !EnumLabel         -- what was being parsed (human-readable label)
      !Text           -- raw value
  | CharsetParseError
      !FilePath
      !Text           -- description of the parse failure
  | UnresolvedName
      !FilePath
      !RowNumber
      !ColumnName       -- the column containing the name
      !Text           -- the name that failed to resolve
  | DanglingEvolution
      !FilePath
      !DexNumber        -- dex number not found in the species map
  | EvolutionCycle
      !FilePath
      !DexNumber        -- species where the cycle was detected
  deriving (Eq, Show)


-- ── Rendering ─────────────────────────────────────────────────────

-- | Format: filepath:rowNumber: description (row-level)
--           filepath: description (file-level)
renderLoadError :: LoadError -> Text
renderLoadError (EmptyCSV errorFilePath) =
  Text.pack errorFilePath <> ": CSV file is empty"

renderLoadError (MissingColumn errorFilePath errorColumnName existingColumns) =
  Text.pack errorFilePath <> ": column not found: "
    <> unColumnName errorColumnName
    <> " (have: " <> Text.intercalate ", " (map unColumnName existingColumns) <> ")"

renderLoadError (UnparseableInt errorFilePath errorRowNumber errorColumnName errorRawValue) =
  Text.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": unparseable integer in column " <> unColumnName errorColumnName
    <> ": " <> quoteValue errorRawValue

renderLoadError (EmptyRequiredField errorFilePath errorRowNumber errorColumnName) =
  Text.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": required field is empty: " <> unColumnName errorColumnName

renderLoadError (RowTooShort errorFilePath errorRowNumber expectedCount actualCount) =
  Text.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": row too short: expected " <> Text.pack (show expectedCount)
    <> " columns, got " <> Text.pack (show actualCount)

renderLoadError (UnknownEnum errorFilePath errorRowNumber errorLabel errorRawValue) =
  Text.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": unknown " <> unEnumLabel errorLabel <> ": " <> quoteValue errorRawValue

renderLoadError (CharsetParseError errorFilePath errorDescription) =
  Text.pack errorFilePath <> ": charset parse error: " <> errorDescription

renderLoadError (UnresolvedName errorFilePath errorRowNumber errorColumnName unresolvedValue) =
  Text.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": unresolved name in column " <> unColumnName errorColumnName
    <> ": " <> quoteValue unresolvedValue

renderLoadError (DanglingEvolution errorFilePath dex) =
  Text.pack errorFilePath <> ": evolution references unknown dex #"
    <> Text.pack (show (unDex dex))

renderLoadError (EvolutionCycle errorFilePath dex) =
  Text.pack errorFilePath <> ": evolution cycle detected at dex #"
    <> Text.pack (show (unDex dex))

renderRowNumber :: RowNumber -> Text
renderRowNumber = Text.pack . show . unRowNumber

quoteValue :: Text -> Text
quoteValue value = "\"" <> value <> "\""


-- ── Convenience ───────────────────────────────────────────────────

-- | Crash with rendered error messages on Left. Used for loading
-- curated data (CSVs, charsets) where errors indicate a build problem,
-- not user input. The save parser has its own error handling via Either
-- SaveError. Returns IO so it chains cleanly with =<< on IO (Either ...) actions.
loadOrDie :: Either [LoadError] a -> IO a
loadOrDie (Right value) = pure value
loadOrDie (Left errors) = error $ Text.unpack $
  Text.intercalate "\n" (map renderLoadError errors)
