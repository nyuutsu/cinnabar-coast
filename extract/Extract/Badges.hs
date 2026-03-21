{-# LANGUAGE OverloadedStrings #-}

-- | Extract badge names and town/spawn point names from pret ASM
-- sources.
--
-- Badges use const_def blocks in ram_constants.asm. Gen 1 badge
-- constants have a BIT_ prefix that gets stripped. Gen 2 has two
-- labeled const_def blocks (wJohtoBadges and wKantoBadges); the
-- parser seeks to each label and parses the block independently,
-- then offsets Kanto indices by the Johto count.
--
-- Gen 1 towns use map_const macros (not plain const) in
-- map_constants.asm, stopping at the DEF NUM_CITY_MAPS boundary.
-- Gen 2 spawn points use a plain const_def block in
-- map_data_constants.asm.

module Extract.Badges
  ( -- * Badges
    extractGen1Badges
  , extractGen2Badges
  , badgesHeader

    -- * Towns / Spawns
  , extractGen1Towns
  , extractGen2Spawns
  , townsHeader
  , spawnsHeader
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec

import Extract.ASM


-- ── Badges ──────────────────────────────────────────────────

-- | Extract Gen 1 badge bit indices from ram_constants.asm.
-- Filters for BIT_*BADGE entries and strips the BIT_ prefix.
extractGen1Badges :: FilePath -> IO [[Text]]
extractGen1Badges path = do
  allConsts <- parseFile parseConstBlock path
  let badges = sortOn fst
        [ (bitIndex, stripBitPrefix name)
        | (name, bitIndex) <- Map.toList allConsts
        , "BIT_" `Text.isPrefixOf` name
        , "BADGE" `Text.isSuffixOf` name
        ]
  pure [formatRow bitIndex badgeName | (bitIndex, badgeName) <- badges]

-- | Extract Gen 2 badge bit indices from ram_constants.asm.
-- Parses the wJohtoBadges and wKantoBadges const_def blocks
-- separately, then offsets Kanto indices by the Johto count so
-- decodeNamedBitFlags over a 2-byte ByteString works directly.
extractGen2Badges :: FilePath -> IO [[Text]]
extractGen2Badges path = do
  (johtoBadges, kantoBadges) <- parseFile parseBadgeBlocks path
  let kantoBadgeOffset = length johtoBadges
      allBadges = johtoBadges
               ++ [(idx + kantoBadgeOffset, name) | (idx, name) <- kantoBadges]
  pure [formatRow bitIndex badgeName | (bitIndex, badgeName) <- allBadges]

badgesHeader :: [Text]
badgesHeader = ["bit_index", "badge_name"]


-- ── Towns / Spawns ──────────────────────────────────────────

-- | Extract Gen 1 town names from map_constants.asm.
-- Collects map_const entries up to the DEF NUM_CITY_MAPS boundary
-- (PALLET_TOWN through SAFFRON_CITY), which correspond to
-- wTownVisitedFlag bits 0-10.
extractGen1Towns :: FilePath -> IO [[Text]]
extractGen1Towns path = do
  towns <- parseFile parseTownMapConsts path
  pure [formatRow bitIndex townName | (bitIndex, townName) <- zip [0..] towns]

-- | Extract Gen 2 spawn point indices from map_data_constants.asm.
-- Filters for SPAWN_* entries from the SpawnPoints const_def block.
extractGen2Spawns :: FilePath -> IO [[Text]]
extractGen2Spawns path = do
  allConsts <- parseFile parseConstBlock path
  let spawns = sortOn fst
        [ (bitIndex, name)
        | (name, bitIndex) <- Map.toList allConsts
        , "SPAWN_" `Text.isPrefixOf` name
        ]
  pure [formatRow bitIndex spawnName | (bitIndex, spawnName) <- spawns]

townsHeader :: [Text]
townsHeader = ["bit_index", "town_name"]

spawnsHeader :: [Text]
spawnsHeader = ["bit_index", "spawn_name"]


-- ── Parsers ─────────────────────────────────────────────────

-- | Parse the wJohtoBadges and wKantoBadges const_def blocks from
-- Gen 2 ram_constants.asm. Returns (johto, kanto) entry lists,
-- each as (bit_index, name) pairs with indices starting at 0.
-- The two blocks are identified by their section label comments
-- ("; wJohtoBadges::" and "; wKantoBadges::").
parseBadgeBlocks :: Parser ([(Int, Text)], [(Int, Text)])
parseBadgeBlocks = do
  skipToLineContaining "wJohtoBadges"
  johtoBadges <- parseConstEntriesUntil (keyword "DEF")
  skipToLineContaining "wKantoBadges"
  kantoBadges <- parseConstEntriesUntil (keyword "DEF")
  pure (johtoBadges, kantoBadges)

-- | Parse map_const entries up to the DEF NUM_CITY_MAPS boundary.
-- Uses map_const macros (not const directives), so the shared
-- const infrastructure from ASM.hs doesn't apply here.
-- Only town/city maps appear before this boundary in
-- map_constants.asm; routes and indoor maps come after.
parseTownMapConsts :: Parser [Text]
parseTownMapConsts = collectTowns []
  where
    collectTowns results = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse results)
        else do
          horizontalSpace
          choice
            [ try $ keyword "DEF" *> keyword "NUM_CITY_MAPS"
                *> pure (reverse results)
            , try $ do
                _ <- keyword "map_const"
                name <- identifier
                restOfLine
                collectTowns (name : results)
            , restOfLine >> collectTowns results
            ]

-- | Skip lines until one contains the given substring.
-- Fails at end of file if the marker is not found.
skipToLineContaining :: Text -> Parser ()
skipToLineContaining needle = do
  line <- takeWhileP Nothing (/= '\n')
  if needle `Text.isInfixOf` line
    then restOfLine
    else do
      _ <- single '\n'
      skipToLineContaining needle


-- ── Helpers ─────────────────────────────────────────────────

stripBitPrefix :: Text -> Text
stripBitPrefix name = fromMaybe name (Text.stripPrefix "BIT_" name)

formatRow :: Int -> Text -> [Text]
formatRow index name = [Text.pack (show index), name]
