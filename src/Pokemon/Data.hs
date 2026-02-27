{-# LANGUAGE OverloadedStrings #-}

-- | Load game data from CSV files into a 'GameData' value.
--
-- Each CSV is simple (no quoted fields, no embedded commas). We parse
-- them directly with Data.Text rather than pulling in a CSV library.
-- If a field can't be parsed, we crash — the CSVs are curated and
-- a parse error means something is genuinely wrong.

module Pokemon.Data
  ( loadGameData
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Types


-- ── Public API ──────────────────────────────────────────────────

-- | Load all static game data for one generation.
loadGameData :: Gen -> IO GameData
loadGameData gen = do
  dir <- getDataDir
  let csv name = dir </> "csv" </> name

  species  <- loadSpecies gen  (csv "species.csv")
  moves    <- loadMoves gen    (csv "moves.csv")
  machines <- loadMachines gen (csv "tmhm.csv")
  compat   <- loadCompat gen   (csv "tmhm_compat.csv")
  levelUp  <- loadLearnsets gen (csv "learnsets.csv")
  eggMoves <- case gen of
    Gen2 -> loadPairMap (csv "egg_moves.csv")
    _    -> pure Map.empty
  tutors   <- case gen of
    Gen2 -> loadPairMap (csv "tutor.csv")
    _    -> pure Map.empty
  items    <- case gen of
    Gen2 -> loadItems (csv "items.csv")
    _    -> pure Map.empty
  evolutions <- loadEvolutions gen (csv "evolutions.csv")

  let evolvesInto = Map.fromListWith (++)
        [ (stepFrom e, [e]) | e <- evolutions ]
      evolvesFrom = Map.fromListWith (++)
        [ (stepTo e, [e]) | e <- evolutions ]

  pure GameData
    { gameGen           = gen
    , gameSpecies       = species
    , gameMoves         = moves
    , gameMachines      = machines
    , gameMachineCompat = compat
    , gameLevelUp       = levelUp
    , gameEggMoves      = eggMoves
    , gameTutorMoves    = tutors
    , gameItems         = items
    , gameEvolvesInto   = evolvesInto
    , gameEvolvesFrom   = evolvesFrom
    }


-- ── CSV reading ─────────────────────────────────────────────────

-- | A parsed CSV file: header index map plus data rows.
data CSV = CSV
  { csvHeader :: !(Map.Map T.Text Int)
  , csvRows   :: ![[T.Text]]
  }

-- | Read a CSV file, parsing the header row into a column-name-to-index
-- map and returning all data rows. Blank lines are silently skipped.
readCSV :: FilePath -> IO CSV
readCSV path = do
  content <- T.readFile path
  let allLines = filter (not . T.null . T.strip) (T.lines content)
  case allLines of
    [] -> error $ "Empty CSV file: " ++ path
    (headerLine:dataLines) ->
      let headers   = map T.strip (T.splitOn "," headerLine)
          headerMap = Map.fromList (zip headers [0..])
          rows = [ map T.strip (T.splitOn "," line) | line <- dataLines ]
      in pure CSV { csvHeader = headerMap, csvRows = rows }

-- | Look up a column name in the CSV header, returning a row accessor.
-- The map lookup happens once; the returned function does only list
-- indexing per row. Crashes immediately if the column doesn't exist.
col :: CSV -> T.Text -> ([T.Text] -> T.Text)
col csv' name =
  case Map.lookup name (csvHeader csv') of
    Nothing -> error $ "CSV column not found: " ++ T.unpack name
                    ++ " (have: " ++ T.unpack (T.intercalate ", " (Map.keys (csvHeader csv'))) ++ ")"
    Just i  -> \row -> row !! i


-- ── Field parsers ───────────────────────────────────────────────

-- | Parse a Text field as Int. Empty/whitespace → 0 (convention for
-- absent optional numeric fields). Non-empty garbage → crash.
int :: T.Text -> Int
int t
  | T.null (T.strip t) = 0
  | otherwise = case reads (T.unpack t) of
      [(n, "")] -> n
      _         -> error $ "Unparseable integer: " ++ T.unpack t

-- | Parse a Text field as Maybe Int. Empty → Nothing.
maybeInt :: T.Text -> Maybe Int
maybeInt t
  | T.null (T.strip t) = Nothing
  | otherwise = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Filter CSV rows where the "gen" column matches a gen number.
forGen :: Gen -> CSV -> [[T.Text]]
forGen gen csv' = filter match (csvRows csv')
  where
    genN = fromEnum gen + 1
    genCol = col csv' "gen"
    match row = int (genCol row) == genN


-- ── Type ID mapping ─────────────────────────────────────────────

-- Game Boy internal type IDs → our PokemonType enum.
-- There's a gap from 0x0A to 0x13 (unused in the games).
typeFromId :: Int -> PokemonType
typeFromId  0 = Normal
typeFromId  1 = Fighting
typeFromId  2 = Flying
typeFromId  3 = Poison
typeFromId  4 = Ground
typeFromId  5 = Rock
typeFromId  7 = Bug
typeFromId  8 = Ghost
typeFromId  9 = Steel
typeFromId 20 = Fire
typeFromId 21 = Water
typeFromId 22 = Grass
typeFromId 23 = Electric
typeFromId 24 = Psychic
typeFromId 25 = Ice
typeFromId 26 = Dragon
typeFromId 27 = Dark
typeFromId n  = error $ "Unknown type ID: " ++ show n

growthFromId :: Int -> GrowthRate
growthFromId 0 = MediumFast
growthFromId 3 = MediumSlow
growthFromId 4 = Fast
growthFromId 5 = Slow
growthFromId n = error $ "Unknown growth rate: " ++ show n


-- ── Loaders ─────────────────────────────────────────────────────

-- | species.csv → Map dex Species
loadSpecies :: Gen -> FilePath -> IO (Map.Map Int Species)
loadSpecies gen path = do
  csv' <- readCSV path
  let dex           = col csv' "dex"
      name          = col csv' "name"
      hp            = col csv' "hp"
      atk           = col csv' "atk"
      dfn           = col csv' "dfn"
      spd           = col csv' "spd"
      spc           = col csv' "spc"
      spa           = col csv' "spa"
      spDef         = col csv' "sp_def"
      type1         = col csv' "type1"
      type2         = col csv' "type2"
      catchRate     = col csv' "catch_rate"
      growthRate    = col csv' "growth_rate"
      genderRatio   = col csv' "gender_ratio"
      eggGroup1     = col csv' "egg_group1"
      eggGroup2     = col csv' "egg_group2"
      baseHappiness = col csv' "base_happiness"
      matching      = forGen gen csv'
  pure $ Map.fromList
    [ (speciesDex sp, sp)
    | row <- matching
    , let sp = Species
            { speciesDex           = int (dex row)
            , speciesName          = T.strip (name row)
            , speciesBaseStats     = BaseStats
                { baseHP      = int (hp row)
                , baseAttack  = int (atk row)
                , baseDefense = int (dfn row)
                , baseSpeed   = int (spd row)
                , baseSpecial = case gen of
                    Gen1 -> Unified (int (spc row))
                    Gen2 -> Split   (int (spa row)) (int (spDef row))
                }
            , speciesTypes         = (typeFromId (int (type1 row)), typeFromId (int (type2 row)))
            , speciesCatchRate     = int (catchRate row)
            , speciesGrowthRate    = growthFromId (int (growthRate row))
            , speciesGenderRatio   = maybeInt (genderRatio row)
            , speciesEggGroups     = case (maybeInt (eggGroup1 row), maybeInt (eggGroup2 row)) of
                (Just a, Just b) -> Just (a, b)
                _                -> Nothing
            , speciesBaseHappiness = maybeInt (baseHappiness row)
            }
    ]


-- | moves.csv → Map moveId Move
loadMoves :: Gen -> FilePath -> IO (Map.Map Int Move)
loadMoves gen path = do
  csv' <- readCSV path
  let mid      = col csv' "id"
      name     = col csv' "name"
      typ      = col csv' "type"
      power    = col csv' "power"
      accuracy = col csv' "accuracy"
      pp       = col csv' "pp"
      matching = forGen gen csv'
  pure $ Map.fromList
    [ (moveId mv, mv)
    | row <- matching
    , let mv = Move
            { moveId       = int (mid row)
            , moveName     = T.strip (name row)
            , moveType     = typeFromId (int (typ row))
            , movePower    = int (power row)
            , moveAccuracy = int (accuracy row)
            , movePP       = int (pp row)
            }
    ]


-- | tmhm.csv → Map Machine moveId
loadMachines :: Gen -> FilePath -> IO (Map.Map Machine Int)
loadMachines gen path = do
  csv' <- readCSV path
  let number  = col csv' "number"
      moveId' = col csv' "move_id"
      isHm    = col csv' "is_hm"
      matching = forGen gen csv'
  pure $ Map.fromList
    [ (machine, int (moveId' row))
    | row <- matching
    , let num     = int (number row)
          machine = if int (isHm row) == 1 then HM num else TM num
    ]


-- | tmhm_compat.csv → Map dex (Set Machine)
-- Numbers 1–50 are TMs; 51+ are HMs (51=HM01, 52=HM02, etc.)
loadCompat :: Gen -> FilePath -> IO (Map.Map Int (Set.Set Machine))
loadCompat gen path = do
  csv' <- readCSV path
  let dex      = col csv' "dex"
      number   = col csv' "number"
      matching = forGen gen csv'
      pairs    = [ (int (dex row), numToMachine (int (number row)))
                 | row <- matching
                 ]
  pure $ Map.fromListWith Set.union
    [ (d, Set.singleton m)
    | (d, m) <- pairs
    ]

numToMachine :: Int -> Machine
numToMachine n
  | n <= 50   = TM n
  | otherwise = HM (n - 50)


-- | learnsets.csv → Map dex [(level, moveId)]
loadLearnsets :: Gen -> FilePath -> IO (Map.Map Int [(Int, Int)])
loadLearnsets gen path = do
  csv' <- readCSV path
  let dex     = col csv' "dex"
      level   = col csv' "level"
      moveId' = col csv' "move_id"
      matching = forGen gen csv'
      triples  = [ (int (dex row), (int (level row), int (moveId' row)))
                 | row <- matching
                 ]
  pure $ Map.fromListWith (++)
    [ (d, [entry])
    | (d, entry) <- triples
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- No gen column — these are Gen 2 only.
loadPairMap :: FilePath -> IO (Map.Map Int (Set.Set Int))
loadPairMap path = do
  csv' <- readCSV path
  let dex     = col csv' "dex"
      moveId' = col csv' "move_id"
  pure $ Map.fromListWith Set.union
    [ (int (dex row), Set.singleton (int (moveId' row)))
    | row <- csvRows csv'
    ]


-- | items.csv → Map itemId name
loadItems :: FilePath -> IO (Map.Map Int T.Text)
loadItems path = do
  csv' <- readCSV path
  let gen  = col csv' "gen"
      mid  = col csv' "id"
      name = col csv' "name"
  pure $ Map.fromList
    [ (int (mid row), T.strip (name row))
    | row <- csvRows csv'
    , int (gen row) == 2
    ]


-- | evolutions.csv → [EvolutionStep]
loadEvolutions :: Gen -> FilePath -> IO [EvolutionStep]
loadEvolutions gen path = do
  csv' <- readCSV path
  let fromDex = col csv' "from_dex"
      toDex   = col csv' "to_dex"
      method  = col csv' "method"
      param   = col csv' "param"
      matching = forGen gen csv'
  pure [ EvolutionStep
           { stepFrom    = int (fromDex row)
           , stepTo      = int (toDex row)
           , stepTrigger = parseTrigger (method row) (param row)
           }
       | row <- matching
       ]

parseTrigger :: T.Text -> T.Text -> EvoTrigger
parseTrigger method param = case method of
  "level"           -> EvoLevel (int param)
  "item"            -> EvoItem (int param)
  "trade"           -> EvoTrade
  "trade_item"      -> EvoTradeItem (int param)
  "happiness"       -> EvoHappiness
  "happiness_day"   -> EvoHappinessDay
  "happiness_night" -> EvoHappinessNight
  "stat_lt"         -> EvoStatLT (int param)
  "stat_gt"         -> EvoStatGT (int param)
  "stat_eq"         -> EvoStatEQ (int param)
  _                 -> error $ "Unknown evolution method: " ++ T.unpack method
