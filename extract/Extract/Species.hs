{-# LANGUAGE OverloadedStrings #-}

-- | Extract species base stats and TM/HM compatibility from
-- pret's data/pokemon/base_stats/*.asm files.
--
-- Each species is a separate file with positional db/dn/tmhm lines.
-- The master base_stats.asm lists INCLUDEs in dex order, so
-- iteration order = dex number.
--
-- Produces two datasets:
--   1. Species CSV: base stats, types, catch rate, growth rate, etc.
--   2. TM/HM compatibility: which TM/HM moves each species can learn.

module Extract.Species
  ( SpeciesData(..)
  , extractSpeciesFile
  , parseBaseStatsIncludes
  , formatGen1Species
  , formatGen2Species
  , speciesHeader
  , tmhmCompatHeader
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Extract.ASM


-- | Parsed base stats for one species.
data SpeciesData = SpeciesData
  { speciesDbArgs  :: ![Text]   -- ^ All db args flattened, in file order
  , speciesDnArgs  :: ![Text]   -- ^ All dn args flattened (Gen 2 egg groups)
  , speciesTmhm    :: ![Text]   -- ^ tmhm macro move names
  } deriving (Show)

-- | Parse the master base_stats.asm to get the INCLUDE filenames in order.
-- Returns filenames like "data/pokemon/base_stats/bulbasaur.asm".
parseBaseStatsIncludes :: Parser [Text]
parseBaseStatsIncludes = go []
  where
    go acc = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse acc)
        else do
          horizontalSpace
          choice
            [ try (parseInclude >>= \path -> go (path : acc))
            , skipLine >> go acc
            ]

    parseInclude = do
      _ <- keyword "INCLUDE"
      _ <- single '"'
      path <- takeWhileP (Just "path") (/= '"')
      _ <- single '"'
      restOfLine
      pure path

    skipLine = takeWhileP Nothing (/= '\n') *> endOfLine
    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

-- | Parse one species base_stats file.
extractSpeciesFile :: FilePath -> IO SpeciesData
extractSpeciesFile = parseFile parseBaseStats

-- | Scan a base_stats file, collecting db/dn/tmhm arguments.
-- Skips INCBIN, dw, comments, blank lines.
parseBaseStats :: Parser SpeciesData
parseBaseStats = go [] [] []
  where
    go dbs dns tmhms = do
      done <- option False (True <$ eof)
      if done
        then pure (SpeciesData
          (concat (reverse dbs))
          (concat (reverse dns))
          (concat (reverse tmhms)))
        else do
          horizontalSpace
          choice
            [ try (parseDbLine >>= \args -> go (args : dbs) dns tmhms)
            , try (parseDnLine >>= \args -> go dbs (args : dns) tmhms)
            , try (parseTmhmLine >>= \args -> go dbs dns (args : tmhms))
            , skipLine >> go dbs dns tmhms
            ]

    parseDbLine = keyword "db" *> commaSeparated
    parseDnLine = keyword "dn" *> commaSeparated

    -- tmhm args may span multiple lines via backslash continuation.
    parseTmhmLine = do
      _ <- keyword "tmhm"
      content <- logicalLine
      pure (map T.strip (filter (not . T.null) (T.splitOn "," content)))

    -- Read a logical line, joining backslash-continued lines.
    logicalLine :: Parser Text
    logicalLine = do
      part <- takeWhileP Nothing (\c -> c /= '\n' && c /= ';' && c /= '\\')
      choice
        [ try $ do
            _ <- chunk "\\"
            endOfLine
            horizontalSpace
            rest <- logicalLine
            pure (part <> rest)
        , do
            _ <- takeWhileP Nothing (/= '\n')
            endOfLine
            pure part
        ]

    skipLine = takeWhileP Nothing (/= '\n') *> endOfLine


-- | Format a Gen 1 species row from parsed data.
-- Gen 1 db args order: DEX, hp, atk, def, spd, spc,
--   TYPE1, TYPE2, catch_rate, base_exp, MOVE1..4, GROWTH_RATE, 0
formatGen1Species :: Int -> SpeciesData -> [Text]
formatGen1Species dex dat =
  let a = speciesDbArgs dat
  in [ "1", T.pack (show dex)
     , a !! 1, a !! 2, a !! 3, a !! 4  -- hp, atk, def, spd
     , a !! 5, a !! 5, a !! 5          -- spc = spa = spd (unified)
     , a !! 6, a !! 7                   -- type1, type2
     , a !! 8, a !! 9                   -- catch_rate, base_exp
     , a !! 14                          -- growth_rate
     , "", "", "", "", "", ""           -- Gen 2 fields absent
     ]

-- | Format a Gen 2 species row from parsed data.
-- Gen 2 db args order: SPECIES, hp, atk, def, spd, spa, spd,
--   TYPE1, TYPE2, catch_rate, base_exp, ITEM1, ITEM2, GENDER,
--   base_happiness, hatch_cycles, unknown, GROWTH_RATE
formatGen2Species :: Int -> SpeciesData -> [Text]
formatGen2Species dex dat =
  let a  = speciesDbArgs dat
      dn = speciesDnArgs dat
      egg1 = if length dn >= 1 then dn !! 0 else ""
      egg2 = if length dn >= 2 then dn !! 1 else ""
  in [ "2", T.pack (show dex)
     , a !! 1, a !! 2, a !! 3, a !! 4  -- hp, atk, def, spd
     , a !! 5, a !! 5, a !! 6          -- spc=spa, spa, spd
     , a !! 7, a !! 8                   -- type1, type2
     , a !! 9, a !! 10                  -- catch_rate, base_exp
     , a !! 17                          -- growth_rate
     , a !! 13                          -- gender_ratio
     , egg1, egg2                       -- egg groups
     , a !! 11, a !! 12                 -- items
     , a !! 15                          -- hatch_cycles
     , a !! 14                          -- base_happiness
     ]

speciesHeader :: [Text]
speciesHeader =
  [ "gen", "dex"
  , "hp", "atk", "dfn", "spd"
  , "spc", "spa", "sp_def"
  , "type1", "type2"
  , "catch_rate", "base_exp", "growth_rate"
  , "gender_ratio", "egg_group1", "egg_group2"
  , "item1", "item2", "hatch_cycles", "base_happiness"
  ]

tmhmCompatHeader :: [Text]
tmhmCompatHeader = ["gen", "dex", "number"]
