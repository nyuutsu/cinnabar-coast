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
  , gen1StartingMoves
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
--   DEX, hp, attack, defense, speed, special,
--   TYPE1, TYPE2, catch_rate, base_exp,
--   4× starting moves, GROWTH_RATE, padding
formatGen1Species :: Text -> Int -> SpeciesData -> [Text]
formatGen1Species name dex dat = case speciesDbArgs dat of
  (_dex : hp : attack : defense : speed : special
    : type1 : type2 : catchRate : baseExp
    : _move1 : _move2 : _move3 : _move4
    : growthRate : _padding : _) ->
    [ "1", T.pack (show dex), name
    , hp, attack, defense, speed
    , special, special, special     -- unified in Gen 1
    , type1, type2
    , catchRate, baseExp, growthRate
    , "", "", "", "", "", "", ""    -- Gen 2 fields absent
    ]
  _ -> error $ "Gen 1 species " ++ T.unpack name ++ ": unexpected field count"

-- | Format a Gen 2 species row from parsed data.
-- Gen 2 db args order (positional):
--   SPECIES, hp, attack, defense, speed,
--   special_attack, special_defense,
--   TYPE1, TYPE2, catch_rate, base_exp,
--   ITEM1, ITEM2, GENDER,
--   base_happiness, hatch_cycles, unknown, GROWTH_RATE
formatGen2Species :: Text -> Int -> SpeciesData -> [Text]
formatGen2Species name dex dat =
  let (eggGroup1, eggGroup2) = case speciesDnArgs dat of
        []         -> ("", "")
        [e1]       -> (e1, "")
        (e1:e2:_)  -> (e1, e2)
  in case speciesDbArgs dat of
    (_species : hp : attack : defense : speed : specialAttack : specialDefense
      : type1 : type2 : catchRate : baseExp
      : item1 : item2 : gender
      : baseHappiness : hatchCycles : _unknown : growthRate : _) ->
      [ "2", T.pack (show dex), name
      , hp, attack, defense, speed
      , specialAttack, specialAttack, specialDefense
      , type1, type2
      , catchRate, baseExp, growthRate
      , gender
      , eggGroup1, eggGroup2
      , item1, item2
      , hatchCycles, baseHappiness
      ]
    _ -> error $ "Gen 2 species " ++ T.unpack name ++ ": unexpected field count"

-- | Extract Gen 1 starting moves from the base_stats db args.
-- Returns move constant names, filtering out NO_MOVE (empty slots).
-- Gen 2 has these inline in evos_attacks.asm; Gen 1 stores them separately.
gen1StartingMoves :: SpeciesData -> [Text]
gen1StartingMoves dat = case speciesDbArgs dat of
  (_dex : _hp : _atk : _def : _spd : _spc
    : _type1 : _type2 : _catchRate : _baseExp
    : move1 : move2 : move3 : move4 : _) ->
    filter (/= "NO_MOVE") [move1, move2, move3, move4]
  _ -> []

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
