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

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Extract.ASM


-- | Parsed data for one species.
data EvosAttacksData = EvosAttacksData
  { evosAttacksEvolutions :: ![[Text]]       -- ^ Each evolution: raw db args [METHOD, ..., TARGET]
  , evosAttacksLearnset   :: ![(Text, Text)] -- ^ (level, move_name) pairs
  } deriving (Show)

-- | Parse an evos_moves/evos_attacks file.
-- Returns (label, data) pairs for every labeled block.
parseEvosAttacksFile :: Parser [(Text, EvosAttacksData)]
parseEvosAttacksFile = scanBlocks []
  where
    scanBlocks blocks = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse blocks)
        else do
          horizontalSpace
          choice
            [ try $ do
                name <- parseLabel
                evos <- parseSection
                moves <- parseMoveSection
                scanBlocks ((name, EvosAttacksData evos moves) : blocks)
            , restOfLine >> scanBlocks blocks
            ]

    -- A label: identifier characters up to ':', then consume rest of line.
    parseLabel = do
      name <- takeWhile1P (Just "label")
                (\char -> char /= ':' && char /= '\n' && char /= ' ' && char /= '\t')
      _ <- single ':'
      restOfLine
      pure name

    -- Parse db lines until we see `db 0` (the section terminator).
    -- Returns the arguments of each non-terminator db line.
    parseSection = collectEntries []
      where
        collectEntries entries = do
          horizontalSpace
          choice
            [ try $ do
                args <- parseDb
                if args == ["0"]
                  then pure (reverse entries)
                  else collectEntries (args : entries)
            , restOfLine >> collectEntries entries
            ]

    -- Parse the learnset section: db lines of (level, move) pairs
    -- until db 0. Validates each entry has exactly 2 fields.
    parseMoveSection = collectMoves []
      where
        collectMoves moves = do
          horizontalSpace
          choice
            [ try $ do
                args <- parseDb
                case args of
                  ["0"] -> pure (reverse moves)
                  [level, move] -> collectMoves ((level, move) : moves)
                  _ -> error $ "unexpected learnset entry: " ++ show args
            , restOfLine >> collectMoves moves
            ]

    parseDb = keyword "db" *> commaSeparated


-- | Format learnset data into CSV rows.
formatLearnsetRows :: Text -> [(Int, EvosAttacksData)] -> [[Text]]
formatLearnsetRows gen blocks =
  [ [gen, T.pack (show dex), level, move]
  | (dex, evosData) <- blocks
  , (level, move) <- evosAttacksLearnset evosData
  ]

-- | Format evolution data into CSV rows.
-- Evolution entries have variable args: [METHOD, params..., TARGET].
-- We normalize to: gen, from_dex, method, target, param1, param2
-- (padding with empty strings for shorter entries).
formatEvolutionRows :: Text -> Map Text Int -> [(Int, EvosAttacksData)] -> [[Text]]
formatEvolutionRows gen speciesToDex blocks =
  [ formatEvoRow gen fromDex evoArgs
  | (fromDex, evosData) <- blocks
  , evoArgs <- evosAttacksEvolutions evosData
  ]
  where
    formatEvoRow :: Text -> Int -> [Text] -> [Text]
    formatEvoRow genLabel fromDex args =
        let buildRow target method param1 param2 =
              let toDex = Map.findWithDefault 0 target speciesToDex
                  -- pret uses EVOLVE_TRADE for both plain trade and trade-with-item.
                  -- Normalize: numeric/empty param1 = plain trade, item name = trade-with-item.
                  (normalizedMethod, normalizedParam1) = case method of
                    "EVOLVE_TRADE"
                      | isNumericOrEmpty param1 -> ("EVOLVE_TRADE", "")
                      | otherwise               -> ("EVOLVE_TRADE_ITEM", param1)
                    _ -> (method, param1)
              in [genLabel, T.pack (show fromDex), T.pack (show toDex), normalizedMethod, normalizedParam1, param2]
        in case args of
          [method, target]                       -> buildRow target method "" ""
          [method, firstParam, target]           -> buildRow target method firstParam ""
          [method, firstParam, secondParam, target] -> buildRow target method firstParam secondParam
          _ -> error $ "unexpected evolution entry: " ++ show args

    -- | Is this text empty, or all digits (possibly with leading minus)?
    -- Used to distinguish pret's numeric sentinel from item constant names.
    isNumericOrEmpty :: Text -> Bool
    isNumericOrEmpty text = case T.uncons (T.strip text) of
      Nothing          -> True
      Just ('-', digits) -> T.all isDigit digits && not (T.null digits)
      Just _           -> T.all isDigit (T.strip text)

learnsetHeader :: [Text]
learnsetHeader = ["gen", "dex", "level", "move_name"]

evolutionHeader :: [Text]
evolutionHeader = ["gen", "from_dex", "to_dex", "method", "param1", "param2"]
