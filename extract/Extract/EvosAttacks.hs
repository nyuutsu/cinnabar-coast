{-# LANGUAGE OverloadedStrings #-}

-- | Parse pret's evos_moves.asm (Gen 1) / evos_attacks.asm (Gen 2).
--
-- Each species has a labeled block containing:
--   1. Evolution entries (variable format per method), terminated by db 0
--   2. Level-up learnset (db level, MOVE_NAME pairs), terminated by db 0
--
-- This module parses both sections and returns them keyed by label.
-- The orchestrator handles label → dex number mapping (which differs
-- between gens because Gen 1 uses scrambled internal indices).

module Extract.EvosAttacks
  ( EvosAttacksData(..)
  , parseEvosAttacksFile
  , formatLearnsetRows
  , formatEvolutionRows
  , learnsetHeader
  , evolutionHeader
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Extract.ASM


-- | Parsed data for one species.
data EvosAttacksData = EvosAttacksData
  { evolutions :: ![[Text]]       -- ^ Each evolution: raw db args [METHOD, ..., TARGET]
  , learnset   :: ![(Text, Text)] -- ^ (level, move_name) pairs
  } deriving (Show)

-- | Parse an evos_moves/evos_attacks file.
-- Returns (label, data) pairs for every labeled block.
parseEvosAttacksFile :: Parser [(Text, EvosAttacksData)]
parseEvosAttacksFile = go []
  where
    go acc = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse acc)
        else do
          horizontalSpace
          choice
            [ try $ do
                name <- parseLabel
                evos <- parseSection
                moves <- parseMoveSection
                go ((name, EvosAttacksData evos moves) : acc)
            , skipLine >> go acc
            ]

    -- A label: identifier characters up to ':', then consume rest of line.
    parseLabel = do
      name <- takeWhile1P (Just "label")
                (\c -> c /= ':' && c /= '\n' && c /= ' ' && c /= '\t')
      _ <- single ':'
      restOfLine
      pure name

    -- Parse db lines until we see `db 0` (the section terminator).
    -- Returns the arguments of each non-terminator db line.
    parseSection = go' []
      where
        go' acc = do
          horizontalSpace
          choice
            [ try $ do
                args <- parseDb
                if args == ["0"]
                  then pure (reverse acc)
                  else go' (args : acc)
            , skipLine >> go' acc
            ]

    -- Parse the learnset section: db lines of (level, move) pairs
    -- until db 0. Validates each entry has exactly 2 fields.
    parseMoveSection = go' []
      where
        go' acc = do
          horizontalSpace
          choice
            [ try $ do
                args <- parseDb
                case args of
                  ["0"] -> pure (reverse acc)
                  [level, move] -> go' ((level, move) : acc)
                  _ -> error $ "unexpected learnset entry: " ++ show args
            , skipLine >> go' acc
            ]

    parseDb = keyword "db" *> commaSeparated
    skipLine = takeWhileP Nothing (/= '\n') *> endOfLine
    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine


-- | Format learnset data into CSV rows.
-- Takes gen label and pre-joined (dex, data) pairs.
formatLearnsetRows :: Text -> [(Int, EvosAttacksData)] -> [[Text]]
formatLearnsetRows gen blocks =
  [ [gen, T.pack (show dex), level, move]
  | (dex, dat) <- blocks
  , (level, move) <- learnset dat
  ]

-- | Format evolution data into CSV rows.
-- Evolution entries have variable args: [METHOD, params..., TARGET].
-- We normalize to: gen, from_dex, method, target, param1, param2
-- (padding with empty strings for shorter entries).
formatEvolutionRows :: Text -> Map Text Int -> [(Int, EvosAttacksData)] -> [[Text]]
formatEvolutionRows gen speciesToDex blocks =
  [ formatEvoRow gen fromDex evoArgs
  | (fromDex, dat) <- blocks
  , evoArgs <- evolutions dat
  ]
  where
    formatEvoRow :: Text -> Int -> [Text] -> [Text]
    formatEvoRow g fromDex args = case args of
      (method : rest) ->
        let target = last rest
            params = init rest   -- middle arguments (between method and target)
            toDex  = Map.findWithDefault 0 target speciesToDex
            param1 = if length params >= 1 then params !! 0 else ""
            param2 = if length params >= 2 then params !! 1 else ""
        in [g, T.pack (show fromDex), T.pack (show toDex), method, param1, param2]
      [] -> error "empty evolution entry"

learnsetHeader :: [Text]
learnsetHeader = ["gen", "dex", "level", "move_name"]

evolutionHeader :: [Text]
evolutionHeader = ["gen", "from_dex", "to_dex", "method", "param1", "param2"]
