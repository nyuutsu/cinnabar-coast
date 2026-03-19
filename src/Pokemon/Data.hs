{-# LANGUAGE OverloadedStrings #-}
-- TODO: Remove -Wno-unused-top-binds after builders are migrated to typed accessors
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  , loadAllGameData
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Error (LoadError (..))
import Pokemon.Types


-- ── Public API ──────────────────────────────────────────────────

-- | Load all static game data for one generation.
-- Convenience wrapper around 'loadAllGameData' for when only one
-- gen is needed. All CSVs are read eagerly, but only the requested
-- gen's GameData is assembled.
loadGameData :: Gen -> IO GameData
loadGameData gen = do
  (gen1Data, gen2Data) <- loadAllGameData
  pure $ case gen of
    Gen1 -> gen1Data
    Gen2 -> gen2Data

-- | Load all static game data for both generations in a single pass.
-- Reads each CSV file once, then partitions rows by gen and builds
-- both GameData values from the shared parse. Returns (Gen 1, Gen 2).
loadAllGameData :: IO (GameData, GameData)
loadAllGameData = do
  dataDir <- getDataDir
  let csvPath name = dataDir </> "csv" </> name

  movesCsv      <- readCSV (csvPath "moves.csv")
  speciesCsv    <- readCSV (csvPath "species.csv")
  machinesCsv   <- readCSV (csvPath "tmhm.csv")
  compatCsv     <- readCSV (csvPath "tmhm_compat.csv")
  learnsetsCsv  <- readCSV (csvPath "learnsets.csv")
  eggMovesCsv   <- readCSV (csvPath "egg_moves.csv")
  tutorCsv      <- readCSV (csvPath "tutor.csv")
  itemsCsv      <- readCSV (csvPath "items.csv")
  evolutionsCsv <- readCSV (csvPath "evolutions.csv")

  let buildForGen gen =
        let (moves, moveNameToId)      = buildMoves gen movesCsv
            (species, speciesNameToId) = buildSpecies gen speciesCsv
            machines   = buildMachines gen moveNameToId machinesCsv
            compat     = buildCompat gen compatCsv
            levelUp    = buildLearnsets gen moveNameToId learnsetsCsv
            eggMoves   = case gen of
              Gen2 -> buildNamePairMap moveNameToId eggMovesCsv
              Gen1 -> Map.empty
            tutors     = case gen of
              Gen2 -> buildNamePairMap moveNameToId tutorCsv
              Gen1 -> Map.empty
            items      = buildItems gen itemsCsv
            evolutions = buildEvolutions gen evolutionsCsv
            evolvesInto = Map.fromListWith (++)
              [ (stepFrom step, [step]) | step <- evolutions ]
            evolvesFrom = Map.fromListWith (++)
              [ (stepTo step, [step]) | step <- evolutions ]
        in GameData
          { gameGen          = gen
          , gameMachineData  = MachineData
              { gameMachines      = machines
              , gameMachineCompat = compat
              }
          , gameLearnsetData = LearnsetData
              { gameLevelUp    = levelUp
              , gameEggMoves   = eggMoves
              , gameTutorMoves = tutors
              }
          , gameSpeciesGraph = SpeciesGraph
              { gameSpecies       = species
              , gameSpeciesByName = speciesNameToId
              , gameEvolvesInto   = evolvesInto
              , gameEvolvesFrom   = evolvesFrom
              }
          , gameLookupTables = LookupTables
              { gameMoves      = moves
              , gameMoveByName = moveNameToId
              , gameItems      = items
              }
          }

  pure (buildForGen Gen1, buildForGen Gen2)


-- ── CSV reading ─────────────────────────────────────────────────

-- | One CSV row, carrying the file path and line number it came from.
-- Column values are keyed by ColumnName, not bare Text.
data Row = Row
  { rowFilePath :: !FilePath
  , rowNumber   :: !RowNumber
  , rowFields   :: !(Map.Map ColumnName T.Text)
  }

-- | A parsed CSV file: source path, column names, and provenance-stamped rows.
data CSV = CSV
  { csvFilePath    :: !FilePath
  , csvColumnNames :: ![ColumnName]
  , csvRows        :: ![Row]
  }

-- | Read a CSV file. Each data row becomes a Row stamped with its source
-- file path and 1-based line number (counting only data rows, not the header).
-- Rows with fewer fields than the header get empty strings for the missing
-- columns. Blank lines are silently skipped.
readCSV :: FilePath -> IO CSV
readCSV path = do
  content <- TIO.readFile path
  let allLines = filter (not . T.null . T.strip) (T.lines content)
  case allLines of
    [] -> error $ "Empty CSV file: " ++ path
    (headerLine:dataLines) ->
      let headers = map (ColumnName . T.strip) (T.splitOn "," headerLine)
          toRow lineNumber line =
            let values = map T.strip (T.splitOn "," line)
                fields = Map.fromList (zip headers (values ++ repeat ""))
            in Row { rowFilePath = path
                   , rowNumber   = RowNumber lineNumber
                   , rowFields   = fields
                   }
          rows = zipWith toRow [1..] dataLines
      in pure CSV { csvFilePath = path, csvColumnNames = headers, csvRows = rows }

-- | Build a row accessor for a named column. Validates that the column
-- exists upfront (crashes immediately on typo), then returns a function
-- that extracts the value from any row via Map lookup.
column :: CSV -> ColumnName -> (Row -> T.Text)
column csv name
  | name `elem` csvColumnNames csv =
      \row -> Map.findWithDefault "" name (rowFields row)
  | otherwise =
      error $ "CSV column not found: " ++ T.unpack (unColumnName name)
           ++ " (have: " ++ T.unpack (T.intercalate ", " (map unColumnName (csvColumnNames csv))) ++ ")"


-- ── Typed column accessors ────────────────────────────────────────

-- | Integer column: empty → EmptyRequiredField, non-numeric → UnparseableInt.
intColumn :: CSV -> ColumnName -> (Row -> Either LoadError Int)
intColumn csv name =
  let accessor = column csv name
  in \row ->
    let rawValue = accessor row
    in if T.null (T.strip rawValue)
       then Left (EmptyRequiredField (rowFilePath row) (rowNumber row) name)
       else case decimal rawValue of
         Right (parsed, remainder)
           | T.null remainder -> Right parsed
         _ -> Left (UnparseableInt (rowFilePath row) (rowNumber row) name rawValue)

-- | Optional integer column: empty → Right Nothing, non-numeric → UnparseableInt.
maybeIntColumn :: CSV -> ColumnName -> (Row -> Either LoadError (Maybe Int))
maybeIntColumn csv name =
  let accessor = column csv name
  in \row ->
    let rawValue = accessor row
    in if T.null (T.strip rawValue)
       then Right Nothing
       else case decimal rawValue of
         Right (parsed, remainder)
           | T.null remainder -> Right (Just parsed)
         _ -> Left (UnparseableInt (rowFilePath row) (rowNumber row) name rawValue)

-- | Required text column: empty → EmptyRequiredField.
textColumn :: CSV -> ColumnName -> (Row -> Either LoadError T.Text)
textColumn csv name =
  let accessor = column csv name
  in \row ->
    let rawValue = accessor row
    in if T.null (T.strip rawValue)
       then Left (EmptyRequiredField (rowFilePath row) (rowNumber row) name)
       else Right rawValue

-- | Optional text column: empty → Right Nothing.
maybeTextColumn :: CSV -> ColumnName -> (Row -> Either LoadError (Maybe T.Text))
maybeTextColumn csv name =
  let accessor = column csv name
  in \row ->
    let rawValue = accessor row
    in if T.null (T.strip rawValue)
       then Right Nothing
       else Right (Just rawValue)


-- ── Field parsers ───────────────────────────────────────────────

-- | Parse a Text field as Int. Empty/whitespace → 0 (convention for
-- absent optional numeric fields). Non-empty garbage → crash.
fieldInt :: T.Text -> Int
fieldInt fieldText
  | T.null (T.strip fieldText) = 0
  | otherwise = case reads (T.unpack fieldText) of
      [(value, "")] -> value
      _         -> error $ "Unparseable integer: " ++ T.unpack fieldText

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
-- Only called for Gen 2 rows where the field is always present.
genderFromName :: T.Text -> GenderRatio
genderFromName "GENDER_F0"      = AllMale
genderFromName "GENDER_F12_5"   = Female12_5
genderFromName "GENDER_F25"     = Female25
genderFromName "GENDER_F50"     = Female50
genderFromName "GENDER_F75"     = Female75
genderFromName "GENDER_F100"    = AllFemale
genderFromName "GENDER_UNKNOWN" = Genderless
genderFromName unrecognized = error $ "Unknown gender ratio: " ++ T.unpack unrecognized

-- | Map pret ASM egg group constant name → EggGroup.
-- Only called for Gen 2 rows where the field is always present.
eggGroupFromName :: T.Text -> EggGroup
eggGroupFromName "EGG_MONSTER"       = EggMonster
eggGroupFromName "EGG_WATER_1"      = EggWater1
eggGroupFromName "EGG_BUG"          = EggBug
eggGroupFromName "EGG_FLYING"       = EggFlying
eggGroupFromName "EGG_GROUND"       = EggGround
eggGroupFromName "EGG_FAIRY"        = EggFairy
eggGroupFromName "EGG_PLANT"        = EggPlant
eggGroupFromName "EGG_HUMANSHAPE"   = EggHumanShape
eggGroupFromName "EGG_WATER_3"      = EggWater3
eggGroupFromName "EGG_MINERAL"      = EggMineral
eggGroupFromName "EGG_INDETERMINATE" = EggIndeterminate
eggGroupFromName "EGG_WATER_2"      = EggWater2
eggGroupFromName "EGG_DITTO"        = EggDitto
eggGroupFromName "EGG_DRAGON"       = EggDragon
eggGroupFromName "EGG_NONE"         = EggNone
eggGroupFromName unrecognized = error $ "Unknown egg group: " ++ T.unpack unrecognized


-- | Unwrap a type name or crash with the species name and bad value
-- for context. Used by buildSpecies for type1 and type2.
requireType :: T.Text -> T.Text -> PokemonType
requireType speciesName raw = case typeFromName raw of
  Just pokemonType -> pokemonType
  Nothing          -> error $ "Species " ++ T.unpack speciesName
                           ++ ": unknown type " ++ T.unpack raw


-- ── Builders ─────────────────────────────────────────────────────

-- | species.csv → (Map dex Species, Map name dex)
-- Columns use ASM constant names for types, growth rate, etc.
-- Returns both the species map and a name→dex lookup.
buildSpecies :: Gen -> CSV -> (Map.Map DexNumber Species, Map.Map T.Text DexNumber)
buildSpecies gen csv =
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
      speciesList =
        [ species
        | row <- matching
        , let species = Species
                { speciesDex           = DexNumber (fieldInt (dexOfRow row))
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
                                         in TypePair (requireType name (type1OfRow row)) (requireType name (type2OfRow row))
                , speciesCatchRate     = fieldInt (catchRateOfRow row)
                , speciesGrowthRate    = growthFromName (growthRateOfRow row)
                , speciesGenFields     = case gen of
                    Gen1 -> Gen1SpeciesFields
                    Gen2 -> Gen2SpeciesFields
                      { speciesGenderRatio   = genderFromName (genderRatioOfRow row)
                      , speciesEggGroups     = EggGroupPair (eggGroupFromName (eggGroup1OfRow row))
                                                            (eggGroupFromName (eggGroup2OfRow row))
                      , speciesBaseHappiness = fieldInt (baseHappinessOfRow row)
                      }
                }
        ]
      speciesMap = Map.fromList [(speciesDex species, species) | species <- speciesList]
      nameToId   = Map.fromList [(speciesName species, speciesDex species) | species <- speciesList]
  in (speciesMap, nameToId)


-- | moves.csv → (Map moveId Move, Map moveName moveId)
-- Returns both the move map and a name→ID lookup for use by
-- other builders that reference moves by constant name.
buildMoves :: Gen -> CSV -> (Map.Map MoveId Move, Map.Map T.Text MoveId)
buildMoves gen csv =
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
                         { moveId       = MoveId (fieldInt (moveIdOfRow row))
                         , moveName     = T.strip (nameOfRow row)
                         , moveType     = moveTypeFromName (moveTypeOfRow row)
                         , movePower    = fieldInt (powerOfRow row)
                         , moveAccuracy = fieldInt (accuracyOfRow row)
                         , movePP       = fieldInt (ppOfRow row)
                         }
                 ]
      moveMap    = Map.fromList [(moveId move, move) | move <- moveList]
      nameToId   = Map.fromList [(moveName move, moveId move) | move <- moveList]
  in (moveMap, nameToId)


-- | tmhm.csv → Map Machine moveId
-- New schema: gen,number,move_name,kind (kind = "tm"/"hm"/"tutor")
-- Tutors are skipped — they aren't TMs or HMs.
buildMachines :: Gen -> Map.Map T.Text MoveId -> CSV -> Map.Map Machine MoveId
buildMachines gen moveNameToId csv =
  let numberOfRow   = column csv "number"
      moveNameOfRow = column csv "move_name"
      kindOfRow     = column csv "kind"
      matching = forGen gen csv
  in Map.fromList
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
buildCompat :: Gen -> CSV -> Map.Map DexNumber (Set.Set Machine)
buildCompat gen csv =
  let dexOfRow    = column csv "dex"
      numberOfRow = column csv "number"
      matching = forGen gen csv
      pairs    = [ (DexNumber (fieldInt (dexOfRow row)), numToMachine (fieldInt (numberOfRow row)))
                 | row <- matching
                 ]
  in Map.fromListWith Set.union
    [ (dexNumber, Set.singleton machine)
    | (dexNumber, machine) <- pairs
    ]

numToMachine :: Int -> Machine
numToMachine number
  | number <= 50   = TM number
  | otherwise = HM (number - 50)


-- | learnsets.csv → Map dex [LevelUpEntry]
-- New schema: gen,dex,level,move_name (name resolved via lookup map)
buildLearnsets :: Gen -> Map.Map T.Text MoveId -> CSV -> Map.Map DexNumber [LevelUpEntry]
buildLearnsets gen moveNameToId csv =
  let dexOfRow      = column csv "dex"
      levelOfRow    = column csv "level"
      moveNameOfRow = column csv "move_name"
      matching = forGen gen csv
      pairs    = [ (DexNumber (fieldInt (dexOfRow row)),
                    LevelUpEntry (Level (fieldInt (levelOfRow row))) matchedMoveId)
                 | row <- matching
                 , Just matchedMoveId <- [Map.lookup (moveNameOfRow row) moveNameToId]
                 ]
  in Map.fromListWith (++)
    [ (dexNumber, [entry])
    | (dexNumber, entry) <- pairs
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- New schema: dex,move_name (no gen column — Gen 2 only).
-- Move names are resolved to IDs via the lookup map.
buildNamePairMap :: Map.Map T.Text MoveId -> CSV -> Map.Map DexNumber (Set.Set MoveId)
buildNamePairMap moveNameToId csv =
  let dexOfRow      = column csv "dex"
      moveNameOfRow = column csv "move_name"
  in Map.fromListWith Set.union
    [ (DexNumber (fieldInt (dexOfRow row)), Set.singleton matchedMoveId)
    | row <- csvRows csv
    , Just matchedMoveId <- [Map.lookup (moveNameOfRow row) moveNameToId]
    ]


-- | items.csv → Map itemId name
buildItems :: Gen -> CSV -> Map.Map ItemId T.Text
buildItems gen csv =
  let itemIdOfRow = column csv "id"
      nameOfRow   = column csv "name"
      matching    = forGen gen csv
  in Map.fromList
    [ (ItemId (fieldInt (itemIdOfRow row)), T.strip (nameOfRow row))
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
buildEvolutions :: Gen -> CSV -> [EvolutionStep]
buildEvolutions gen csv =
  let fromDexOfRow = column csv "from_dex"
      toDexOfRow   = column csv "to_dex"
      methodOfRow  = column csv "method"
      param1OfRow  = column csv "param1"
      param2OfRow  = column csv "param2"
      matching = forGen gen csv
  in [ EvolutionStep
         { stepFrom    = DexNumber (fieldInt (fromDexOfRow row))
         , stepTo      = DexNumber (fieldInt (toDexOfRow row))
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
    "EVOLVE_LEVEL"      -> EvoLevel (Level (fieldInt rawParam1))
    "EVOLVE_ITEM"       -> EvoItem (T.strip rawParam1)
    "EVOLVE_TRADE"      -> EvoTrade
    "EVOLVE_TRADE_ITEM" -> EvoTradeItem (T.strip rawParam1)
    "EVOLVE_HAPPINESS"  -> case T.strip rawParam1 of
      "TR_ANYTIME" -> EvoHappiness
      "TR_MORNDAY" -> EvoHappinessDay
      "TR_NITE"    -> EvoHappinessNight
      unrecognized -> error $ "Unknown happiness time: " ++ T.unpack unrecognized
    "EVOLVE_STAT" -> case T.strip rawParam2 of
      "ATK_LT_DEF" -> EvoStatLT (Level (fieldInt rawParam1))
      "ATK_GT_DEF" -> EvoStatGT (Level (fieldInt rawParam1))
      "ATK_EQ_DEF" -> EvoStatEQ (Level (fieldInt rawParam1))
      unrecognized -> error $ "Unknown stat comparison: " ++ T.unpack unrecognized
    unrecognized -> error $ "Unknown evolution method: " ++ T.unpack unrecognized
