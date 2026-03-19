{-# LANGUAGE OverloadedStrings #-}

-- | Structured errors for the game data loading layer.
--
-- The pure query layer (Legality, Stats) is total — errors live here,
-- at the boundary where CSVs become domain types. This module defines
-- the error type, a renderer, and a convenience crash function for
-- the current error-or-die loading style.

module Pokemon.Error
  ( LoadError (..)
  , renderLoadError
  , loadOrDie
  ) where

import qualified Data.Text as T

import Pokemon.Types (ColumnName (..), EnumLabel (..), RowNumber (..))


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
      !T.Text           -- raw value
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
      !T.Text           -- raw value
  | CharsetParseError
      !FilePath
      !T.Text           -- description of the parse failure
  deriving (Eq, Show)


-- ── Rendering ─────────────────────────────────────────────────────

-- | Format: filepath:rowNumber: description (row-level)
--           filepath: description (file-level)
renderLoadError :: LoadError -> T.Text
renderLoadError (EmptyCSV errorFilePath) =
  T.pack errorFilePath <> ": CSV file is empty"

renderLoadError (MissingColumn errorFilePath errorColumnName existingColumns) =
  T.pack errorFilePath <> ": column not found: "
    <> unColumnName errorColumnName
    <> " (have: " <> T.intercalate ", " (map unColumnName existingColumns) <> ")"

renderLoadError (UnparseableInt errorFilePath errorRowNumber errorColumnName errorRawValue) =
  T.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": unparseable integer in column " <> unColumnName errorColumnName
    <> ": " <> quoteValue errorRawValue

renderLoadError (EmptyRequiredField errorFilePath errorRowNumber errorColumnName) =
  T.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": required field is empty: " <> unColumnName errorColumnName

renderLoadError (RowTooShort errorFilePath errorRowNumber expectedCount actualCount) =
  T.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": row too short: expected " <> T.pack (show expectedCount)
    <> " columns, got " <> T.pack (show actualCount)

renderLoadError (UnknownEnum errorFilePath errorRowNumber errorLabel errorRawValue) =
  T.pack errorFilePath <> ":" <> renderRowNumber errorRowNumber
    <> ": unknown " <> unEnumLabel errorLabel <> ": " <> quoteValue errorRawValue

renderLoadError (CharsetParseError errorFilePath errorDescription) =
  T.pack errorFilePath <> ": charset parse error: " <> errorDescription

renderRowNumber :: RowNumber -> T.Text
renderRowNumber = T.pack . show . unRowNumber

quoteValue :: T.Text -> T.Text
quoteValue value = "\"" <> value <> "\""


-- ── Convenience ───────────────────────────────────────────────────

-- | Crash with rendered error messages on Left. Intended for the
-- current error-or-die loading style; will be replaced with proper
-- error handling when the save parser introduces untrusted input.
-- Returns IO so it chains cleanly with =<< on IO (Either ...) actions.
loadOrDie :: Either [LoadError] a -> IO a
loadOrDie (Right value) = pure value
loadOrDie (Left errors) = error $ T.unpack $
  T.intercalate "\n" (map renderLoadError errors)
