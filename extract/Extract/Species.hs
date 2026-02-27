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
            , restOfLine >> go acc
            ]

    parseInclude = do
      _ <- keyword "INCLUDE"
      _ <- single '"'
      path <- takeWhileP (Just "path") (/= '"')
      _ <- single '"'
      restOfLine
      pure path

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
            , restOfLine >> go dbs dns tmhms
            ]

    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

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



-- | Format a Gen 1 species row from parsed data.
-- Gen 1 db args order (positional):
--   [0] DEX, [1] hp, [2] attack, [3] defense, [4] speed, [5] special,
--   [6] TYPE1, [7] TYPE2, [8] catch_rate, [9] base_exp,
--   [10..13] starting moves, [14] GROWTH_RATE, [15] padding
formatGen1Species :: Text -> Int -> SpeciesData -> [Text]
formatGen1Species name dex dat =
  let fields = speciesDbArgs dat
  in [ "1", T.pack (show dex), name
     , fields !! 1                              -- hp
     , fields !! 2, fields !! 3, fields !! 4    -- attack, defense, speed
     , fields !! 5, fields !! 5, fields !! 5    -- special (all three columns, unified in Gen 1)
     , fields !! 6, fields !! 7                 -- type1, type2
     , fields !! 8, fields !! 9                 -- catch_rate, base_exp
     , fields !! 14                             -- growth_rate
     , "", "", "", "", "", "", ""               -- Gen 2 fields absent (7 fields)
     ]

-- | Format a Gen 2 species row from parsed data.
-- Gen 2 db args order (positional):
--   [0] SPECIES, [1] hp, [2] attack, [3] defense, [4] speed,
--   [5] special_attack, [6] special_defense,
--   [7] TYPE1, [8] TYPE2, [9] catch_rate, [10] base_exp,
--   [11] ITEM1, [12] ITEM2, [13] GENDER,
--   [14] base_happiness, [15] hatch_cycles, [16] unknown, [17] GROWTH_RATE
formatGen2Species :: Text -> Int -> SpeciesData -> [Text]
formatGen2Species name dex dat =
  let fields = speciesDbArgs dat
      (eggGroup1, eggGroup2) = case speciesDnArgs dat of
        []       -> ("", "")
        [e1]     -> (e1, "")
        (e1:e2:_) -> (e1, e2)
  in [ "2", T.pack (show dex), name
     , fields !! 1                              -- hp
     , fields !! 2, fields !! 3, fields !! 4    -- attack, defense, speed
     , fields !! 5, fields !! 5, fields !! 6    -- special = special_attack, special_attack, special_defense
     , fields !! 7, fields !! 8                 -- type1, type2
     , fields !! 9, fields !! 10                -- catch_rate, base_exp
     , fields !! 17                             -- growth_rate
     , fields !! 13                             -- gender_ratio
     , eggGroup1, eggGroup2                     -- egg groups (from dn directive)
     , fields !! 11, fields !! 12               -- item1, item2
     , fields !! 15                             -- hatch_cycles
     , fields !! 14                             -- base_happiness
     ]

speciesHeader :: [Text]
speciesHeader =
  [ "gen", "dex", "name"
  , "hp", "attack", "defense", "speed"
  , "special", "special_attack", "special_defense"
  , "type1", "type2"
  , "catch_rate", "base_exp", "growth_rate"
  , "gender_ratio", "egg_group1", "egg_group2"
  , "item1", "item2", "hatch_cycles", "base_happiness"
  ]

tmhmCompatHeader :: [Text]
tmhmCompatHeader = ["gen", "dex", "number"]
