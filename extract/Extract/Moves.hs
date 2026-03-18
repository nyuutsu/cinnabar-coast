{-# LANGUAGE OverloadedStrings #-}

-- | Extract move data from pret's data/moves/moves.asm files.
--
-- Each gen's moves.asm contains a table of `move` macro invocations:
--
--   Gen 1: move NAME, EFFECT, POWER, TYPE, ACCURACY, PP
--   Gen 2: move NAME, EFFECT, POWER, TYPE, ACCURACY, PP, EFFECT_CHANCE
--
-- Moves are 1-indexed (POUND = 1) and appear in order. The macro
-- arguments are a mix of constant names (NAME, EFFECT, TYPE) and
-- bare integers (POWER, ACCURACY, PP, EFFECT_CHANCE).
--
-- We extract all fields faithfully — the Haskell loader picks which
-- columns it needs via the column accessor pattern.

module Extract.Moves (extractMoves, movesHeader) where

import Data.Text (Text)
import qualified Data.Text as T
import Extract.ASM


-- | Extract moves from one gen's moves.asm file.
-- Returns CSV rows (no header) as lists of Text fields.
-- Each row: [gen, id, name, effect, power, type, accuracy, pp, effect_chance]
extractMoves :: Text -> FilePath -> IO [[Text]]
extractMoves gen path = do
  moves <- parseFile parseMoveTable path
  pure [formatRow gen moveId fields | (moveId, fields) <- zip [1..] moves]

-- | Scan the file for `move` macro lines, skipping everything else.
parseMoveTable :: Parser [[Text]]
parseMoveTable = scanLines (keyword "move" *> commaSeparated)

-- | Format a parsed move line into a CSV row.
-- Handles both 6-arg (Gen 1) and 7-arg (Gen 2) lines.
formatRow :: Text -> Int -> [Text] -> [Text]
formatRow gen moveId fields = case fields of
  [name, effect, power, moveType, accuracy, pp] ->
    [gen, T.pack (show moveId), name, effect, power, moveType, accuracy, pp, "0"]
  [name, effect, power, moveType, accuracy, pp, effectChance] ->
    [gen, T.pack (show moveId), name, effect, power, moveType, accuracy, pp, effectChance]
  _ -> error $ "move line has unexpected number of fields: " ++ show (length fields)

-- | CSV header for the moves table.
movesHeader :: [Text]
movesHeader = ["gen", "id", "name", "effect", "power", "type", "accuracy", "pp", "effect_chance"]
