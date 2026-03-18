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
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Types


-- ── Public API ──────────────────────────────────────────────────

-- | Load all static game data for one generation.
-- Moves are loaded first because other CSVs reference moves by
-- constant name rather than numeric ID.
loadGameData :: Gen -> IO GameData
loadGameData gen = do
  dataDir <- getDataDir
  let csvPath name = dataDir </> "csv" </> name

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
        [ (stepFrom step, [step]) | step <- evolutions ]
      evolvesFrom = Map.fromListWith (++)
        [ (stepTo step, [step]) | step <- evolutions ]

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

-- | One CSV row: column name → value. Self-describing, no positional
-- indexing. Missing trailing columns produce empty strings.
type Row = Map.Map T.Text T.Text

-- | A parsed CSV file: column names (for error messages) plus rows.
data CSV = CSV
  { csvColumnNames :: ![T.Text]
  , csvRows        :: ![Row]
  }

-- | Read a CSV file. Each data row becomes a Map from column name to
-- value. Rows with fewer fields than the header get empty strings for
-- the missing columns. Blank lines are silently skipped.
readCSV :: FilePath -> IO CSV
readCSV path = do
  content <- TIO.readFile path
  let allLines = filter (not . T.null . T.strip) (T.lines content)
  case allLines of
    [] -> error $ "Empty CSV file: " ++ path
    (headerLine:dataLines) ->
      let headers = map T.strip (T.splitOn "," headerLine)
          toRow line =
            let values = map T.strip (T.splitOn "," line)
            in Map.fromList (zip headers (values ++ repeat ""))
          rows = map toRow dataLines
      in pure CSV { csvColumnNames = headers, csvRows = rows }

-- | Build a row accessor for a named column. Validates that the column
-- exists upfront (crashes immediately on typo), then returns a function
-- that extracts the value from any row via Map lookup.
column :: CSV -> T.Text -> (Row -> T.Text)
column csv name
  | name `elem` csvColumnNames csv =
      \row -> Map.findWithDefault "" name row
  | otherwise =
      error $ "CSV column not found: " ++ T.unpack name
           ++ " (have: " ++ T.unpack (T.intercalate ", " (csvColumnNames csv)) ++ ")"


-- ── Field parsers ───────────────────────────────────────────────

-- | Parse a Text field as Int. Empty/whitespace → 0 (convention for
-- absent optional numeric fields). Non-empty garbage → crash.
fieldInt :: T.Text -> Int
fieldInt fieldText
  | T.null (T.strip fieldText) = 0
  | otherwise = case reads (T.unpack fieldText) of
      [(value, "")] -> value
      _         -> error $ "Unparseable integer: " ++ T.unpack fieldText

-- | Parse a Text field as Maybe Int. Empty → Nothing.
maybeInt :: T.Text -> Maybe Int
maybeInt fieldText
  | T.null (T.strip fieldText) = Nothing
  | otherwise = case reads (T.unpack fieldText) of
      [(value, "")] -> Just value
      _             -> Nothing

-- | Filter CSV rows where the "gen" column matches a gen number.
forGen :: Gen -> CSV -> [Row]
forGen gen csv = filter matchesGen (csvRows csv)
  where
    genNumber = fromEnum gen + 1
    genOfRow = column csv "gen"
    matchesGen row = fieldInt (genOfRow row) == genNumber


-- ── Name-based mapping ──────────────────────────────────────────

-- | Map pret ASM type constant name → PokemonType.
-- Returns Nothing for unrecognized names so callers can provide
-- context-specific error messages.
typeFromName :: T.Text -> Maybe PokemonType
typeFromName "NORMAL"       = Just Normal
typeFromName "FIGHTING"     = Just Fighting
typeFromName "FLYING"       = Just Flying
typeFromName "POISON"       = Just Poison
typeFromName "GROUND"       = Just Ground
typeFromName "ROCK"         = Just Rock
typeFromName "BUG"          = Just Bug
typeFromName "GHOST"        = Just Ghost
typeFromName "STEEL"        = Just Steel
typeFromName "FIRE"         = Just Fire
typeFromName "WATER"        = Just Water
typeFromName "GRASS"        = Just Grass
typeFromName "ELECTRIC"     = Just Electric
typeFromName "PSYCHIC"      = Just Psychic
typeFromName "PSYCHIC_TYPE" = Just Psychic  -- pret alias (avoids collision with move name)
typeFromName "ICE"          = Just Ice
typeFromName "DRAGON"       = Just Dragon
typeFromName "DARK"         = Just Dark
typeFromName _              = Nothing

-- | Map pret ASM type constant name → MoveType.
-- Handles CURSE_TYPE (the ??? type used only by Curse) and delegates
-- to typeFromName for all standard element types.
moveTypeFromName :: T.Text -> MoveType
moveTypeFromName "CURSE_TYPE" = CurseType
moveTypeFromName name = case typeFromName name of
  Just pokemonType -> StandardType pokemonType
  Nothing          -> error $ "Unknown move type: " ++ T.unpack name

-- | Map pret ASM growth rate constant name → GrowthRate.
growthFromName :: T.Text -> GrowthRate
growthFromName "GROWTH_MEDIUM_FAST" = MediumFast
growthFromName "GROWTH_MEDIUM_SLOW" = MediumSlow
growthFromName "GROWTH_FAST"        = Fast
growthFromName "GROWTH_SLOW"        = Slow
growthFromName unrecognized = error $ "Unknown growth rate: " ++ T.unpack unrecognized

-- | Map pret ASM gender ratio constant name → GenderRatio.
-- Empty string (Gen 1, no gender) → Nothing.
genderFromName :: T.Text -> Maybe GenderRatio
genderFromName ""               = Nothing
genderFromName "GENDER_F0"      = Just AllMale
genderFromName "GENDER_F12_5"   = Just Female12_5
genderFromName "GENDER_F25"     = Just Female25
genderFromName "GENDER_F50"     = Just Female50
genderFromName "GENDER_F75"     = Just Female75
genderFromName "GENDER_F100"    = Just AllFemale
genderFromName "GENDER_UNKNOWN" = Just Genderless
genderFromName unrecognized = error $ "Unknown gender ratio: " ++ T.unpack unrecognized

-- | Map pret ASM egg group constant name → EggGroup.
-- Empty string (Gen 1, no egg groups) → Nothing.
eggGroupFromName :: T.Text -> Maybe EggGroup
eggGroupFromName ""                  = Nothing
eggGroupFromName "EGG_MONSTER"       = Just EggMonster
eggGroupFromName "EGG_WATER_1"      = Just EggWater1
eggGroupFromName "EGG_BUG"          = Just EggBug
eggGroupFromName "EGG_FLYING"       = Just EggFlying
eggGroupFromName "EGG_GROUND"       = Just EggGround
eggGroupFromName "EGG_FAIRY"        = Just EggFairy
eggGroupFromName "EGG_PLANT"        = Just EggPlant
eggGroupFromName "EGG_HUMANSHAPE"   = Just EggHumanShape
eggGroupFromName "EGG_WATER_3"      = Just EggWater3
eggGroupFromName "EGG_MINERAL"      = Just EggMineral
eggGroupFromName "EGG_INDETERMINATE" = Just EggIndeterminate
eggGroupFromName "EGG_WATER_2"      = Just EggWater2
eggGroupFromName "EGG_DITTO"        = Just EggDitto
eggGroupFromName "EGG_DRAGON"       = Just EggDragon
eggGroupFromName "EGG_NONE"         = Just EggNone
eggGroupFromName unrecognized = error $ "Unknown egg group: " ++ T.unpack unrecognized


-- | Unwrap a type name or crash with the species name and bad value
-- for context. Used by loadSpecies for type1 and type2.
requireType :: T.Text -> T.Text -> PokemonType
requireType speciesName raw = case typeFromName raw of
  Just pokemonType -> pokemonType
  Nothing          -> error $ "Species " ++ T.unpack speciesName
                           ++ ": unknown type " ++ T.unpack raw


-- ── Loaders ─────────────────────────────────────────────────────

-- | species.csv → Map dex Species
-- Columns use ASM constant names for types, growth rate, etc.
loadSpecies :: Gen -> FilePath -> IO (Map.Map Int Species)
loadSpecies gen path = do
  csv <- readCSV path
  let dexOfRow              = column csv "dex"
      nameOfRow             = column csv "name"
      hpOfRow               = column csv "hp"
      attackOfRow           = column csv "attack"
      defenseOfRow          = column csv "defense"
      speedOfRow            = column csv "speed"
      specialOfRow          = column csv "special"
      specialAttackOfRow    = column csv "special_attack"
      specialDefenseOfRow   = column csv "special_defense"
      type1OfRow            = column csv "type1"
      type2OfRow            = column csv "type2"
      catchRateOfRow        = column csv "catch_rate"
      growthRateOfRow       = column csv "growth_rate"
      genderRatioOfRow      = column csv "gender_ratio"
      eggGroup1OfRow        = column csv "egg_group1"
      eggGroup2OfRow        = column csv "egg_group2"
      baseHappinessOfRow    = column csv "base_happiness"
      matching      = forGen gen csv
  pure $ Map.fromList
    [ (speciesDex species, species)
    | row <- matching
    , let species = Species
            { speciesDex           = fieldInt (dexOfRow row)
            , speciesName          = T.strip (nameOfRow row)
            , speciesBaseStats     = BaseStats
                { baseHP      = fieldInt (hpOfRow row)
                , baseAttack  = fieldInt (attackOfRow row)
                , baseDefense = fieldInt (defenseOfRow row)
                , baseSpeed   = fieldInt (speedOfRow row)
                , baseSpecial = case gen of
                    Gen1 -> Unified (fieldInt (specialOfRow row))
                    Gen2 -> Split   (fieldInt (specialAttackOfRow row)) (fieldInt (specialDefenseOfRow row))
                }
            , speciesTypes         = let name = nameOfRow row
                                     in (requireType name (type1OfRow row), requireType name (type2OfRow row))
            , speciesCatchRate     = fieldInt (catchRateOfRow row)
            , speciesGrowthRate    = growthFromName (growthRateOfRow row)
            , speciesGenderRatio   = genderFromName (genderRatioOfRow row)
            , speciesEggGroups     = case (eggGroupFromName (eggGroup1OfRow row), eggGroupFromName (eggGroup2OfRow row)) of
                (Just group1, Just group2) -> Just (group1, group2)
                _                          -> Nothing
            , speciesBaseHappiness = maybeInt (baseHappinessOfRow row)
            }
    ]


-- | moves.csv → (Map moveId Move, Map moveName moveId)
-- Returns both the move map and a name→ID lookup for use by
-- other loaders that reference moves by constant name.
loadMoves :: Gen -> FilePath -> IO (Map.Map Int Move, Map.Map T.Text Int)
loadMoves gen path = do
  csv <- readCSV path
  let moveIdOfRow   = column csv "id"
      nameOfRow     = column csv "name"
      moveTypeOfRow = column csv "type"
      powerOfRow    = column csv "power"
      accuracyOfRow = column csv "accuracy"
      ppOfRow       = column csv "pp"
      matching = forGen gen csv
      moveList = [ move
                 | row <- matching
                 , let move = Move
                         { moveId       = fieldInt (moveIdOfRow row)
                         , moveName     = T.strip (nameOfRow row)
                         , moveType     = moveTypeFromName (moveTypeOfRow row)
                         , movePower    = fieldInt (powerOfRow row)
                         , moveAccuracy = fieldInt (accuracyOfRow row)
                         , movePP       = fieldInt (ppOfRow row)
                         }
                 ]
      moveMap    = Map.fromList [(moveId move, move) | move <- moveList]
      nameToId   = Map.fromList [(moveName move, moveId move) | move <- moveList]
  pure (moveMap, nameToId)


-- | tmhm.csv → Map Machine moveId
-- New schema: gen,number,move_name,kind (kind = "tm"/"hm"/"tutor")
-- Tutors are skipped — they aren't TMs or HMs.
loadMachines :: Gen -> Map.Map T.Text Int -> FilePath -> IO (Map.Map Machine Int)
loadMachines gen moveNameToId path = do
  csv <- readCSV path
  let numberOfRow   = column csv "number"
      moveNameOfRow = column csv "move_name"
      kindOfRow     = column csv "kind"
      matching = forGen gen csv
  pure $ Map.fromList
    [ (machine, matchedMoveId)
    | row <- matching
    , let machineKind = kindOfRow row
    , machineKind == "tm" || machineKind == "hm"    -- skip tutors
    , let machineNumber = fieldInt (numberOfRow row)
          machine = if machineKind == "hm" then HM machineNumber else TM machineNumber
    , Just matchedMoveId <- [Map.lookup (moveNameOfRow row) moveNameToId]
    ]


-- | tmhm_compat.csv → Map dex (Set Machine)
-- Numbers 1–50 are TMs; 51+ are HMs (51=HM01, 52=HM02, etc.)
loadCompat :: Gen -> FilePath -> IO (Map.Map Int (Set.Set Machine))
loadCompat gen path = do
  csv <- readCSV path
  let dexOfRow    = column csv "dex"
      numberOfRow = column csv "number"
      matching = forGen gen csv
      pairs    = [ (fieldInt (dexOfRow row), numToMachine (fieldInt (numberOfRow row)))
                 | row <- matching
                 ]
  pure $ Map.fromListWith Set.union
    [ (dexNumber, Set.singleton machine)
    | (dexNumber, machine) <- pairs
    ]

numToMachine :: Int -> Machine
numToMachine number
  | number <= 50   = TM number
  | otherwise = HM (number - 50)


-- | learnsets.csv → Map dex [(level, moveId)]
-- New schema: gen,dex,level,move_name (name resolved via lookup map)
loadLearnsets :: Gen -> Map.Map T.Text Int -> FilePath -> IO (Map.Map Int [(Int, Int)])
loadLearnsets gen moveNameToId path = do
  csv <- readCSV path
  let dexOfRow      = column csv "dex"
      levelOfRow    = column csv "level"
      moveNameOfRow = column csv "move_name"
      matching = forGen gen csv
      triples  = [ (fieldInt (dexOfRow row), (fieldInt (levelOfRow row), matchedMoveId))
                 | row <- matching
                 , Just matchedMoveId <- [Map.lookup (moveNameOfRow row) moveNameToId]
                 ]
  pure $ Map.fromListWith (++)
    [ (dexNumber, [entry])
    | (dexNumber, entry) <- triples
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- New schema: dex,move_name (no gen column — Gen 2 only).
-- Move names are resolved to IDs via the lookup map.
loadNamePairMap :: Map.Map T.Text Int -> FilePath -> IO (Map.Map Int (Set.Set Int))
loadNamePairMap moveNameToId path = do
  csv <- readCSV path
  let dexOfRow      = column csv "dex"
      moveNameOfRow = column csv "move_name"
  pure $ Map.fromListWith Set.union
    [ (fieldInt (dexOfRow row), Set.singleton matchedMoveId)
    | row <- csvRows csv
    , Just matchedMoveId <- [Map.lookup (moveNameOfRow row) moveNameToId]
    ]


-- | items.csv → Map itemId name
loadItems :: Gen -> FilePath -> IO (Map.Map Int T.Text)
loadItems gen path = do
  csv <- readCSV path
  let itemIdOfRow = column csv "id"
      nameOfRow   = column csv "name"
      matching    = forGen gen csv
  pure $ Map.fromList
    [ (fieldInt (itemIdOfRow row), T.strip (nameOfRow row))
    | row <- matching
    ]


-- | Raw CSV fields for one evolution row, before parsing into an EvoTrigger.
data RawEvolution = RawEvolution
  { rawMethod :: !T.Text
  , rawParam1 :: !T.Text
  , rawParam2 :: !T.Text
  }

-- | evolutions.csv → [EvolutionStep]
-- New schema: gen,from_dex,to_dex,method,param1,param2
-- Methods and params use pret ASM constant names.
loadEvolutions :: Gen -> FilePath -> IO [EvolutionStep]
loadEvolutions gen path = do
  csv <- readCSV path
  let fromDexOfRow = column csv "from_dex"
      toDexOfRow   = column csv "to_dex"
      methodOfRow  = column csv "method"
      param1OfRow  = column csv "param1"
      param2OfRow  = column csv "param2"
      matching = forGen gen csv
  pure [ EvolutionStep
           { stepFrom    = fieldInt (fromDexOfRow row)
           , stepTo      = fieldInt (toDexOfRow row)
           , stepTrigger = parseTrigger RawEvolution
               { rawMethod = methodOfRow row
               , rawParam1 = param1OfRow row
               , rawParam2 = param2OfRow row
               }
           }
       | row <- matching
       ]

-- | Parse an evolution trigger from ASM constant names.
-- EVOLVE_TRADE and EVOLVE_TRADE_ITEM are distinct in the CSV
-- (normalized at extraction time), so no heuristic is needed.
parseTrigger :: RawEvolution -> EvoTrigger
parseTrigger RawEvolution{rawMethod, rawParam1, rawParam2} =
  case T.strip rawMethod of
    "EVOLVE_LEVEL"      -> EvoLevel (fieldInt rawParam1)
    "EVOLVE_ITEM"       -> EvoItem (T.strip rawParam1)
    "EVOLVE_TRADE"      -> EvoTrade
    "EVOLVE_TRADE_ITEM" -> EvoTradeItem (T.strip rawParam1)
    "EVOLVE_HAPPINESS"  -> case T.strip rawParam1 of
      "TR_ANYTIME" -> EvoHappiness
      "TR_MORNDAY" -> EvoHappinessDay
      "TR_NITE"    -> EvoHappinessNight
      unrecognized -> error $ "Unknown happiness time: " ++ T.unpack unrecognized
    "EVOLVE_STAT" -> case T.strip rawParam2 of
      "ATK_LT_DEF" -> EvoStatLT (fieldInt rawParam1)
      "ATK_GT_DEF" -> EvoStatGT (fieldInt rawParam1)
      "ATK_EQ_DEF" -> EvoStatEQ (fieldInt rawParam1)
      unrecognized -> error $ "Unknown stat comparison: " ++ T.unpack unrecognized
    unrecognized -> error $ "Unknown evolution method: " ++ T.unpack unrecognized
