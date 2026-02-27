{-# LANGUAGE OverloadedStrings #-}

-- | Load game data from CSV files into a 'GameData' value.
--
-- Each CSV is simple (no quoted fields, no embedded commas). We parse
-- them directly with Data.Text rather than pulling in a CSV library.
-- If a field can't be parsed, we crash — the CSVs are curated and
-- a parse error means something is genuinely wrong.
--
-- The CSVs store pret ASM constant names (NORMAL, FIRE, MOON_STONE,
-- GROWTH_MEDIUM_SLOW, etc.) rather than numeric IDs. This module maps
-- those names to our domain types.

module Pokemon.Data
  ( loadGameData
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isDigit)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Types


-- ── Public API ──────────────────────────────────────────────────

-- | Load all static game data for one generation.
-- Moves are loaded first because other CSVs reference moves by
-- constant name rather than numeric ID.
loadGameData :: Gen -> IO GameData
loadGameData gen = do
  dir <- getDataDir
  let csvPath name = dir </> "csv" </> name

  -- Step 1: Load moves (other CSVs reference moves by name)
  (moves, moveNameToId) <- loadMoves gen (csvPath "moves.csv")

  -- Step 2: Load everything else, resolving move names to IDs
  species  <- loadSpecies gen  (csvPath "species.csv")
  machines <- loadMachines gen moveNameToId (csvPath "tmhm.csv")
  compat   <- loadCompat gen   (csvPath "tmhm_compat.csv")
  levelUp  <- loadLearnsets gen moveNameToId (csvPath "learnsets.csv")
  eggMoves <- case gen of
    Gen2 -> loadNamePairMap moveNameToId (csvPath "egg_moves.csv")
    _    -> pure Map.empty
  tutors   <- case gen of
    Gen2 -> loadNamePairMap moveNameToId (csvPath "tutor.csv")
    _    -> pure Map.empty
  items    <- loadItems gen (csvPath "items.csv")
  evolutions <- loadEvolutions gen (csvPath "evolutions.csv")

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
col csv name =
  case Map.lookup name (csvHeader csv) of
    Nothing -> error $ "CSV column not found: " ++ T.unpack name
                    ++ " (have: " ++ T.unpack (T.intercalate ", " (Map.keys (csvHeader csv))) ++ ")"
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
forGen gen csv = filter match (csvRows csv)
  where
    genN = fromEnum gen + 1
    genCol = col csv "gen"
    match row = int (genCol row) == genN

-- | Check if a Text value looks like a number (digits, optionally
-- preceded by a minus sign). Used to distinguish numeric params
-- from constant names in evolution data.
isNumeric :: T.Text -> Bool
isNumeric t = case T.uncons (T.strip t) of
  Nothing       -> True   -- empty = numeric (treated as 0)
  Just ('-', r) -> T.all isDigit r && not (T.null r)
  Just _        -> T.all isDigit (T.strip t)


-- ── Name-based mapping ──────────────────────────────────────────

-- | Map pret ASM type constant name → PokemonType.
-- Used for species types (which are always real element types).
typeFromName :: T.Text -> PokemonType
typeFromName "NORMAL"   = Normal
typeFromName "FIGHTING" = Fighting
typeFromName "FLYING"   = Flying
typeFromName "POISON"   = Poison
typeFromName "GROUND"   = Ground
typeFromName "ROCK"     = Rock
typeFromName "BUG"      = Bug
typeFromName "GHOST"    = Ghost
typeFromName "STEEL"    = Steel
typeFromName "FIRE"     = Fire
typeFromName "WATER"    = Water
typeFromName "GRASS"    = Grass
typeFromName "ELECTRIC" = Electric
typeFromName "PSYCHIC"      = Psychic
typeFromName "PSYCHIC_TYPE" = Psychic  -- pret alias (avoids collision with move name)
typeFromName "ICE"      = Ice
typeFromName "DRAGON"   = Dragon
typeFromName "DARK"     = Dark
typeFromName n          = error $ "Unknown type name: " ++ T.unpack n

-- | Map pret ASM type constant name → MoveType.
-- Handles CURSE_TYPE (the ??? type used only by Curse) and delegates
-- to typeFromName for all standard element types.
moveTypeFromName :: T.Text -> MoveType
moveTypeFromName "CURSE_TYPE" = CurseType
moveTypeFromName n            = StandardType (typeFromName n)

-- | Map pret ASM growth rate constant name → GrowthRate.
growthFromName :: T.Text -> GrowthRate
growthFromName "GROWTH_MEDIUM_FAST" = MediumFast
growthFromName "GROWTH_MEDIUM_SLOW" = MediumSlow
growthFromName "GROWTH_FAST"        = Fast
growthFromName "GROWTH_SLOW"        = Slow
growthFromName n = error $ "Unknown growth rate: " ++ T.unpack n


-- ── Loaders ─────────────────────────────────────────────────────

-- | species.csv → Map dex Species
-- Columns use ASM constant names for types, growth rate, etc.
loadSpecies :: Gen -> FilePath -> IO (Map.Map Int Species)
loadSpecies gen path = do
  csv <- readCSV path
  let dex           = col csv "dex"
      name          = col csv "name"
      hp            = col csv "hp"
      attack        = col csv "attack"
      defense       = col csv "defense"
      speed         = col csv "speed"
      special       = col csv "special"
      specialAttk   = col csv "special_attack"
      specialDef    = col csv "special_defense"
      type1         = col csv "type1"
      type2         = col csv "type2"
      catchRate     = col csv "catch_rate"
      growthRate    = col csv "growth_rate"
      genderRatio   = col csv "gender_ratio"
      eggGroup1     = col csv "egg_group1"
      eggGroup2     = col csv "egg_group2"
      baseHappiness = col csv "base_happiness"
      matching      = forGen gen csv
  pure $ Map.fromList
    [ (speciesDex sp, sp)
    | row <- matching
    , let sp = Species
            { speciesDex           = int (dex row)
            , speciesName          = T.strip (name row)
            , speciesBaseStats     = BaseStats
                { baseHP      = int (hp row)
                , baseAttack  = int (attack row)
                , baseDefense = int (defense row)
                , baseSpeed   = int (speed row)
                , baseSpecial = case gen of
                    Gen1 -> Unified (int (special row))
                    Gen2 -> Split   (int (specialAttk row)) (int (specialDef row))
                }
            , speciesTypes         = (typeFromName (type1 row), typeFromName (type2 row))
            , speciesCatchRate     = int (catchRate row)
            , speciesGrowthRate    = growthFromName (growthRate row)
            -- TODO: gender_ratio and egg_group columns now contain
            -- constant names (GENDER_F50, EGG_MONSTER, etc.) rather
            -- than numeric IDs. maybeInt returns Nothing for these.
            -- Parse them properly when domain logic needs them.
            , speciesGenderRatio   = maybeInt (genderRatio row)
            , speciesEggGroups     = case (maybeInt (eggGroup1 row), maybeInt (eggGroup2 row)) of
                (Just a, Just b) -> Just (a, b)
                _                -> Nothing
            , speciesBaseHappiness = maybeInt (baseHappiness row)
            }
    ]


-- | moves.csv → (Map moveId Move, Map moveName moveId)
-- Returns both the move map and a name→ID lookup for use by
-- other loaders that reference moves by constant name.
loadMoves :: Gen -> FilePath -> IO (Map.Map Int Move, Map.Map T.Text Int)
loadMoves gen path = do
  csv <- readCSV path
  let mid      = col csv "id"
      name     = col csv "name"
      typ      = col csv "type"
      power    = col csv "power"
      accuracy = col csv "accuracy"
      pp       = col csv "pp"
      matching = forGen gen csv
      moveList = [ mv
                 | row <- matching
                 , let mv = Move
                         { moveId       = int (mid row)
                         , moveName     = T.strip (name row)
                         , moveType     = moveTypeFromName (typ row)
                         , movePower    = int (power row)
                         , moveAccuracy = int (accuracy row)
                         , movePP       = int (pp row)
                         }
                 ]
      moveMap    = Map.fromList [(moveId mv, mv) | mv <- moveList]
      nameToId   = Map.fromList [(moveName mv, moveId mv) | mv <- moveList]
  pure (moveMap, nameToId)


-- | tmhm.csv → Map Machine moveId
-- New schema: gen,number,move_name,kind (kind = "tm"/"hm"/"tutor")
-- Tutors are skipped — they aren't TMs or HMs.
loadMachines :: Gen -> Map.Map T.Text Int -> FilePath -> IO (Map.Map Machine Int)
loadMachines gen moveNameToId path = do
  csv <- readCSV path
  let number   = col csv "number"
      moveName = col csv "move_name"
      kind     = col csv "kind"
      matching = forGen gen csv
  pure $ Map.fromList
    [ (machine, mid)
    | row <- matching
    , let k = kind row
    , k == "tm" || k == "hm"    -- skip tutors
    , let num = int (number row)
          machine = if k == "hm" then HM num else TM num
    , Just mid <- [Map.lookup (moveName row) moveNameToId]
    ]


-- | tmhm_compat.csv → Map dex (Set Machine)
-- Numbers 1–50 are TMs; 51+ are HMs (51=HM01, 52=HM02, etc.)
loadCompat :: Gen -> FilePath -> IO (Map.Map Int (Set.Set Machine))
loadCompat gen path = do
  csv <- readCSV path
  let dex      = col csv "dex"
      number   = col csv "number"
      matching = forGen gen csv
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
-- New schema: gen,dex,level,move_name (name resolved via lookup map)
loadLearnsets :: Gen -> Map.Map T.Text Int -> FilePath -> IO (Map.Map Int [(Int, Int)])
loadLearnsets gen moveNameToId path = do
  csv <- readCSV path
  let dex      = col csv "dex"
      level    = col csv "level"
      moveName = col csv "move_name"
      matching = forGen gen csv
      triples  = [ (int (dex row), (int (level row), mid))
                 | row <- matching
                 , Just mid <- [Map.lookup (moveName row) moveNameToId]
                 ]
  pure $ Map.fromListWith (++)
    [ (d, [entry])
    | (d, entry) <- triples
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- New schema: dex,move_name (no gen column — Gen 2 only).
-- Move names are resolved to IDs via the lookup map.
loadNamePairMap :: Map.Map T.Text Int -> FilePath -> IO (Map.Map Int (Set.Set Int))
loadNamePairMap moveNameToId path = do
  csv <- readCSV path
  let dex      = col csv "dex"
      moveName = col csv "move_name"
  pure $ Map.fromListWith Set.union
    [ (int (dex row), Set.singleton mid)
    | row <- csvRows csv
    , Just mid <- [Map.lookup (moveName row) moveNameToId]
    ]


-- | items.csv → Map itemId name
loadItems :: Gen -> FilePath -> IO (Map.Map Int T.Text)
loadItems gen path = do
  csv <- readCSV path
  let genN = fromEnum gen + 1
      genCol = col csv "gen"
      mid  = col csv "id"
      name = col csv "name"
  pure $ Map.fromList
    [ (int (mid row), T.strip (name row))
    | row <- csvRows csv
    , int (genCol row) == genN
    ]


-- | evolutions.csv → [EvolutionStep]
-- New schema: gen,from_dex,to_dex,method,param1,param2
-- Methods and params use pret ASM constant names.
loadEvolutions :: Gen -> FilePath -> IO [EvolutionStep]
loadEvolutions gen path = do
  csv <- readCSV path
  let fromDex = col csv "from_dex"
      toDex   = col csv "to_dex"
      method  = col csv "method"
      param1  = col csv "param1"
      param2  = col csv "param2"
      matching = forGen gen csv
  pure [ EvolutionStep
           { stepFrom    = int (fromDex row)
           , stepTo      = int (toDex row)
           , stepTrigger = parseTrigger (method row) (param1 row) (param2 row)
           }
       | row <- matching
       ]

-- | Parse an evolution trigger from ASM constant names.
parseTrigger :: T.Text -> T.Text -> T.Text -> EvoTrigger
parseTrigger method param1 param2 = case T.strip method of
  "EVOLVE_LEVEL" -> EvoLevel (int param1)
  "EVOLVE_ITEM"  -> EvoItem (T.strip param1)
  "EVOLVE_TRADE" ->
    let p = T.strip param1
    in if isNumeric p then EvoTrade else EvoTradeItem p
  "EVOLVE_HAPPINESS" -> case T.strip param1 of
    "TR_ANYTIME" -> EvoHappiness
    "TR_MORNDAY" -> EvoHappinessDay
    "TR_NITE"    -> EvoHappinessNight
    x -> error $ "Unknown happiness time: " ++ T.unpack x
  "EVOLVE_STAT" -> case T.strip param2 of
    "ATK_LT_DEF" -> EvoStatLT (int param1)
    "ATK_GT_DEF" -> EvoStatGT (int param1)
    "ATK_EQ_DEF" -> EvoStatEQ (int param1)
    x -> error $ "Unknown stat comparison: " ++ T.unpack x
  x -> error $ "Unknown evolution method: " ++ T.unpack x
