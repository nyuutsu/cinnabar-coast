{-# LANGUAGE OverloadedStrings #-}

-- | Load game data from CSV files into a 'GameData' value.
--
-- Each CSV is simple (no quoted fields, no embedded commas). We parse
-- them directly with Data.Text rather than pulling in a CSV library.
-- Structured errors are returned via 'Either' 'LoadError' rather than
-- crashing — the CSVs are curated, but errors are reported cleanly.
--
-- The CSVs store pret ASM constant names (NORMAL, FIRE, MOON_STONE,
-- GROWTH_MEDIUM_SLOW, etc.) rather than numeric IDs. This module maps
-- those names to our domain types.

module Cinnabar.Data
  ( loadGameData
  , loadAllGameData
  ) where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT, except)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read (decimal)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Cinnabar.Error (LoadError (..))
import Cinnabar.Schema (eggGroupNames, genderRatioNames, growthRateNames, typeNames)
import Cinnabar.Types


-- ── Public API ──────────────────────────────────────────────────

-- | Load all static game data for one generation.
-- All CSVs are read and validated; only the requested gen is returned.
loadGameData :: Gen -> IO (Either [LoadError] GameData)
loadGameData gen = do
  result <- loadAllGameData
  pure $ case result of
    Left errors -> Left errors
    Right (gen1Data, gen2Data) -> Right $ case gen of
      Gen1 -> gen1Data
      Gen2 -> gen2Data

-- | Load all static game data for both generations in a single pass.
-- Reads each CSV file once, then builds both GameData values.
-- Returns (Gen 1, Gen 2), or the first error encountered.
loadAllGameData :: IO (Either [LoadError] (GameData, GameData))
loadAllGameData = do
  dataDir <- getDataDir
  let csvPath name = dataDir </> "csv" </> name
  result <- runExceptT $ do
    movesCsv      <- ExceptT (readCSV (csvPath "moves.csv"))
    speciesCsv    <- ExceptT (readCSV (csvPath "species.csv"))
    machinesCsv   <- ExceptT (readCSV (csvPath "tmhm.csv"))
    compatCsv     <- ExceptT (readCSV (csvPath "tmhm_compat.csv"))
    learnsetsCsv  <- ExceptT (readCSV (csvPath "learnsets.csv"))
    eggMovesCsv   <- ExceptT (readCSV (csvPath "egg_moves.csv"))
    tutorCsv      <- ExceptT (readCSV (csvPath "tutor.csv"))
    itemsCsv      <- ExceptT (readCSV (csvPath "items.csv"))
    evolutionsCsv    <- ExceptT (readCSV (csvPath "evolutions.csv"))
    internalIndexCsv <- ExceptT (readCSV (csvPath "internal_index.csv"))
    eventFlagsCsv    <- ExceptT (readCSV (csvPath "event_flags.csv"))
    toggleFlagsCsv   <- ExceptT (readCSV (csvPath "toggle_flags.csv"))
    mapScriptsCsv    <- ExceptT (readCSV (csvPath "map_scripts.csv"))
    badgesCsv        <- ExceptT (readCSV (csvPath "gen1_badges.csv"))
    gymLeadersCsv    <- ExceptT (readCSV (csvPath "gen1_gym_leaders.csv"))
    townsCsv         <- ExceptT (readCSV (csvPath "gen1_towns.csv"))
    tradesRBCsv      <- ExceptT (readCSV (csvPath "gen1rb_trades.csv"))
    tradesYellowCsv  <- ExceptT (readCSV (csvPath "gen1yellow_trades.csv"))

    let buildForGen gen flagNames = do
          (moves, moveNameToId)      <- buildMoves gen movesCsv
          (species, speciesNameToId) <- buildSpecies gen speciesCsv
          machines   <- buildMachines gen moveNameToId machinesCsv
          compat     <- buildCompat gen compatCsv
          levelUp    <- buildLearnsets gen moveNameToId learnsetsCsv
          eggMoves   <- case gen of
            Gen2 -> buildNamePairMap moveNameToId eggMovesCsv
            Gen1 -> Right Map.empty
          tutors     <- case gen of
            Gen2 -> buildNamePairMap moveNameToId tutorCsv
            Gen1 -> Right Map.empty
          items      <- buildItems gen itemsCsv
          internalIndex <- case gen of
            Gen1 -> buildInternalIndex internalIndexCsv
            Gen2 -> Right Map.empty
          evolutions <- buildEvolutions gen evolutionsCsv
          validateEvolutionDexNumbers (csvFilePath evolutionsCsv) species evolutions
          let evolvesInto = Map.fromListWith (++)
                [ (stepFrom step, [step]) | step <- evolutions ]
              evolvesFrom = Map.fromListWith (++)
                [ (stepTo step, [step]) | step <- evolutions ]
          detectEvolutionCycle (csvFilePath evolutionsCsv) evolvesFrom
          pure GameData
            { gameGen          = gen
            , gameMachineData  = MachineData
                { gameMachines      = machines
                , gameMoveToMachine = Map.fromList
                    [ (moveId, machine) | (machine, moveId) <- Map.toList machines ]
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
                , gameInternalIndex = internalIndex
                }
            , gameLookupTables = LookupTables
                { gameMoves      = moves
                , gameMoveByName = moveNameToId
                , gameItems      = items
                }
            , gameGen1FlagNames = flagNames
            }

    except $ do
      gen1FlagNames <- buildGen1FlagNames eventFlagsCsv toggleFlagsCsv mapScriptsCsv
                         badgesCsv gymLeadersCsv townsCsv tradesRBCsv tradesYellowCsv
      gen1Data <- buildForGen Gen1 (Just gen1FlagNames)
      gen2Data <- buildForGen Gen2 Nothing
      pure (gen1Data, gen2Data)

  pure $ case result of
    Left loadError -> Left [loadError]
    Right value    -> Right value


-- ── CSV reading ─────────────────────────────────────────────────

-- | One CSV row, carrying the file path and line number it came from.
-- Column values are keyed by ColumnName, not bare Text.
data Row = Row
  { rowFilePath :: !FilePath
  , rowNumber   :: !RowNumber
  , rowFields   :: !(Map ColumnName Text)
  }

-- | A parsed CSV file: source path, column names, and provenance-stamped rows.
data CSV = CSV
  { csvFilePath    :: !FilePath
  , csvColumnNames :: !(Set ColumnName)
  , csvRows        :: ![Row]
  }

-- | Read a CSV file. Each data row becomes a Row stamped with its source
-- file path and 1-based line number (counting only data rows, not the header).
-- Rows with fewer fields than the header produce a RowTooShort error.
-- Blank lines and comment lines (starting with #) are silently skipped.
readCSV :: FilePath -> IO (Either LoadError CSV)
readCSV path = do
  content <- TextIO.readFile path
  let allLines = filter isContentLine (Text.lines content)
      isContentLine line = let stripped = Text.strip line
                           in not (Text.null stripped)
                           && not ("#" `Text.isPrefixOf` stripped)
  pure $ case allLines of
    [] -> Left (EmptyCSV path)
    (headerLine:dataLines) ->
      let headers = map (ColumnName . Text.strip) (Text.splitOn "," headerLine)
          expectedCount = length headers
      in do
        rows <- sequence (zipWith (buildRow headers expectedCount) [1..] dataLines)
        pure CSV { csvFilePath = path, csvColumnNames = Set.fromList headers, csvRows = rows }
  where
    buildRow headers expectedCount lineNumber line =
      let values = map Text.strip (Text.splitOn "," line)
          actualCount = length values
      in if actualCount < expectedCount
         then Left (RowTooShort path (RowNumber lineNumber) expectedCount actualCount)
         else Right Row
           { rowFilePath = path
           , rowNumber   = RowNumber lineNumber
           , rowFields   = Map.fromList (zip headers values)
           }


-- ── Column accessors ──────────────────────────────────────────────

-- | Build a row accessor for a named column. Validates that the column
-- exists in the header, returning MissingColumn if not. Internal helper
-- used by the typed accessor functions below.
column :: CSV -> ColumnName -> Either LoadError (Row -> Text)
column csv name
  | Set.member name (csvColumnNames csv) =
      Right (\row -> Map.findWithDefault "" name (rowFields row))
  | otherwise =
      Left (MissingColumn (csvFilePath csv) name (Set.toAscList (csvColumnNames csv)))

-- | Integer column: empty → EmptyRequiredField, non-numeric → UnparseableInt.
intColumn :: CSV -> ColumnName -> Either LoadError (Row -> Either LoadError Int)
intColumn csv name = do
  accessor <- column csv name
  pure $ \row ->
    let rawValue = accessor row
    in if Text.null rawValue
       then Left (EmptyRequiredField (rowFilePath row) (rowNumber row) name)
       else case decimal rawValue of
         Right (parsed, remainder)
           | Text.null remainder -> Right parsed
         _ -> Left (UnparseableInt (rowFilePath row) (rowNumber row) name rawValue)

-- | Required text column: empty → EmptyRequiredField.
textColumn :: CSV -> ColumnName -> Either LoadError (Row -> Either LoadError Text)
textColumn csv name = do
  accessor <- column csv name
  pure $ \row ->
    let rawValue = accessor row
    in if Text.null rawValue
       then Left (EmptyRequiredField (rowFilePath row) (rowNumber row) name)
       else Right rawValue

-- | Optional text column: empty → Right Nothing.
maybeTextColumn :: CSV -> ColumnName -> Either LoadError (Row -> Either LoadError (Maybe Text))
maybeTextColumn csv name = do
  accessor <- column csv name
  pure $ \row ->
    let rawValue = accessor row
    in if Text.null rawValue
       then Right Nothing
       else Right (Just rawValue)


-- ── Enum helpers ───────────────────────────────────────────────

-- | Require a name-to-type mapping to succeed, or produce UnknownEnum.
requireEnum :: Row -> EnumLabel -> Map Text a -> Text -> Either LoadError a
requireEnum row label names rawValue = case Map.lookup rawValue names of
  Just result -> Right result
  Nothing     -> Left (UnknownEnum (rowFilePath row) (rowNumber row) label rawValue)

-- | Require a name-to-ID mapping to succeed, or produce UnresolvedName.
requireName :: Row -> ColumnName -> Map Text a -> Text -> Either LoadError a
requireName row columnName names rawValue = case Map.lookup rawValue names of
  Just result -> Right result
  Nothing     -> Left (UnresolvedName (rowFilePath row) (rowNumber row) columnName rawValue)

-- | Look up a move's type. Checks for CURSE_TYPE first (the ???
-- type used only by Curse), then delegates to the standard type map.
lookupMoveType :: Row -> Text -> Either LoadError MoveType
lookupMoveType row rawValue
  | rawValue == "CURSE_TYPE" = Right CurseType
  | otherwise = StandardType <$> requireEnum row "move type" typeNames rawValue


-- ── Row filtering ────────────────────────────────────────────────

-- | Filter CSV rows where the "gen" column matches a gen number.
-- Unparseable gen values produce a LoadError rather than being silently skipped.
forGen :: Gen -> CSV -> Either LoadError [Row]
forGen gen csv = do
  genOfRow <- intColumn csv "gen"
  let genNumber = fromEnum gen + 1
      checkGen row = do
        rowGen <- genOfRow row
        pure $ if rowGen == genNumber then Just row else Nothing
  checked <- traverse checkGen (csvRows csv)
  pure [row | Just row <- checked]


-- ── Builders ─────────────────────────────────────────────────────

-- | species.csv → (Map dex Species, Map name dex)
-- Columns use ASM constant names for types, growth rate, etc.
-- Returns both the species map and a name→dex lookup.
buildSpecies :: Gen -> CSV -> Either LoadError (Map DexNumber Species, Map Text DexNumber)
buildSpecies gen csv = do
  dexOfRow              <- intColumn csv "dex"
  nameOfRow             <- textColumn csv "name"
  hpOfRow               <- intColumn csv "hp"
  attackOfRow           <- intColumn csv "attack"
  defenseOfRow          <- intColumn csv "defense"
  speedOfRow            <- intColumn csv "speed"
  specialOfRow          <- intColumn csv "special"
  specialAttackOfRow    <- intColumn csv "special_attack"
  specialDefenseOfRow   <- intColumn csv "special_defense"
  type1OfRow            <- textColumn csv "type1"
  type2OfRow            <- textColumn csv "type2"
  catchRateOfRow        <- intColumn csv "catch_rate"
  growthRateOfRow       <- textColumn csv "growth_rate"
  genderRatioOfRow      <- textColumn csv "gender_ratio"
  eggGroup1OfRow        <- textColumn csv "egg_group1"
  eggGroup2OfRow        <- textColumn csv "egg_group2"
  baseHappinessOfRow    <- intColumn csv "base_happiness"
  matching              <- forGen gen csv
  let parseSpeciesRow row = do
        dex           <- dexOfRow row
        name          <- nameOfRow row
        hp            <- hpOfRow row
        attack        <- attackOfRow row
        defense       <- defenseOfRow row
        speed         <- speedOfRow row
        special       <- case gen of
          Gen1 -> Unified <$> specialOfRow row
          Gen2 -> Split <$> specialAttackOfRow row <*> specialDefenseOfRow row
        type1Text     <- type1OfRow row
        type2Text     <- type2OfRow row
        type1         <- requireEnum row "type" typeNames type1Text
        type2         <- requireEnum row "type" typeNames type2Text
        catchRate     <- catchRateOfRow row
        growthRateText <- growthRateOfRow row
        growthRate    <- requireEnum row "growth rate" growthRateNames growthRateText
        genFields     <- case gen of
          Gen1 -> pure Gen1SpeciesFields
          Gen2 -> do
            genderRatioText <- genderRatioOfRow row
            genderRatio     <- requireEnum row "gender ratio" genderRatioNames genderRatioText
            eggGroup1Text   <- eggGroup1OfRow row
            eggGroup2Text   <- eggGroup2OfRow row
            eggGroup1       <- requireEnum row "egg group" eggGroupNames eggGroup1Text
            eggGroup2       <- requireEnum row "egg group" eggGroupNames eggGroup2Text
            baseHappiness   <- baseHappinessOfRow row
            pure Gen2SpeciesFields
              { speciesGenderRatio   = genderRatio
              , speciesEggGroups     = EggGroupPair eggGroup1 eggGroup2
              , speciesBaseHappiness = baseHappiness
              }
        pure Species
          { speciesDex       = DexNumber dex
          , speciesName      = name
          , speciesBaseStats = BaseStats
              { baseHP      = hp
              , baseAttack  = attack
              , baseDefense = defense
              , baseSpeed   = speed
              , baseSpecial = special
              }
          , speciesTypes      = TypePair type1 type2
          , speciesCatchRate  = catchRate
          , speciesGrowthRate = growthRate
          , speciesGenFields  = genFields
          }
  speciesList <- traverse parseSpeciesRow matching
  let speciesMap = Map.fromList [(speciesDex species, species) | species <- speciesList]
      nameToId   = Map.fromList [(speciesName species, speciesDex species) | species <- speciesList]
  pure (speciesMap, nameToId)


-- | moves.csv → (Map moveId Move, Map moveName moveId)
-- Returns both the move map and a name→ID lookup for use by
-- other builders that reference moves by constant name.
buildMoves :: Gen -> CSV -> Either LoadError (Map MoveId Move, Map Text MoveId)
buildMoves gen csv = do
  moveIdOfRow   <- intColumn csv "id"
  nameOfRow     <- textColumn csv "name"
  moveTypeOfRow <- textColumn csv "type"
  powerOfRow    <- intColumn csv "power"
  accuracyOfRow <- intColumn csv "accuracy"
  ppOfRow       <- intColumn csv "pp"
  matching      <- forGen gen csv
  let parseMoveRow row = do
        moveIdValue   <- moveIdOfRow row
        name          <- nameOfRow row
        moveTypeText  <- moveTypeOfRow row
        moveTypeValue <- lookupMoveType row moveTypeText
        power         <- powerOfRow row
        accuracy      <- accuracyOfRow row
        pp            <- ppOfRow row
        pure Move
          { moveId       = MoveId moveIdValue
          , moveName     = name
          , moveType     = moveTypeValue
          , movePower    = power
          , moveAccuracy = accuracy
          , movePP       = pp
          }
  moveList <- traverse parseMoveRow matching
  let moveMap  = Map.fromList [(moveId move, move) | move <- moveList]
      nameToId = Map.fromList [(moveName move, moveId move) | move <- moveList]
  pure (moveMap, nameToId)


-- | tmhm.csv → Map Machine moveId
-- New schema: gen,number,move_name,kind (kind = "tm"/"hm"/"tutor")
-- Tutors are skipped — they aren't TMs or HMs.
buildMachines :: Gen -> Map Text MoveId -> CSV -> Either LoadError (Map Machine MoveId)
buildMachines gen moveNameToId csv = do
  numberOfRow   <- intColumn csv "number"
  moveNameOfRow <- textColumn csv "move_name"
  kindOfRow     <- textColumn csv "kind"
  matching      <- forGen gen csv
  let parseMachineRow row = do
        machineKind <- kindOfRow row
        if machineKind /= "tm" && machineKind /= "hm"
          then pure Nothing
          else do
            machineNumber <- numberOfRow row
            moveName <- moveNameOfRow row
            matchedMoveId <- requireName row "move_name" moveNameToId moveName
            let machine = if machineKind == "hm" then HM (MachineNumber machineNumber) else TM (MachineNumber machineNumber)
            pure $ Just (machine, matchedMoveId)
  results <- traverse parseMachineRow matching
  pure $ Map.fromList [pair | Just pair <- results]


-- | tmhm_compat.csv → Map dex (Set Machine)
-- Numbers 1–50 are TMs; 51+ are HMs (51=HM01, 52=HM02, etc.)
buildCompat :: Gen -> CSV -> Either LoadError (Map DexNumber (Set Machine))
buildCompat gen csv = do
  dexOfRow    <- intColumn csv "dex"
  numberOfRow <- intColumn csv "number"
  matching    <- forGen gen csv
  let parseCompatRow row = do
        dex    <- dexOfRow row
        number <- numberOfRow row
        pure (DexNumber dex, numToMachine number)
  pairs <- traverse parseCompatRow matching
  pure $ Map.fromListWith Set.union
    [ (dexNumber, Set.singleton machine)
    | (dexNumber, machine) <- pairs
    ]

numToMachine :: Int -> Machine
numToMachine number
  | number <= 50 = TM (MachineNumber number)
  | otherwise    = HM (MachineNumber (number - 50))


-- | learnsets.csv → Map dex [LevelUpEntry]
-- New schema: gen,dex,level,move_name (name resolved via lookup map)
buildLearnsets :: Gen -> Map Text MoveId -> CSV -> Either LoadError (Map DexNumber [LevelUpEntry])
buildLearnsets gen moveNameToId csv = do
  dexOfRow      <- intColumn csv "dex"
  levelOfRow    <- intColumn csv "level"
  moveNameOfRow <- textColumn csv "move_name"
  matching      <- forGen gen csv
  let parseLearnsetRow row = do
        dex <- dexOfRow row
        level <- levelOfRow row
        moveName <- moveNameOfRow row
        matchedMoveId <- requireName row "move_name" moveNameToId moveName
        pure (DexNumber dex, LevelUpEntry (Level level) matchedMoveId)
  results <- traverse parseLearnsetRow matching
  pure $ Map.fromListWith (++)
    [ (dexNumber, [entry])
    | (dexNumber, entry) <- results
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- New schema: dex,move_name (no gen column — Gen 2 only).
-- Move names are resolved to IDs via the lookup map.
buildNamePairMap :: Map Text MoveId -> CSV -> Either LoadError (Map DexNumber (Set MoveId))
buildNamePairMap moveNameToId csv = do
  dexOfRow      <- intColumn csv "dex"
  moveNameOfRow <- textColumn csv "move_name"
  let parseNamePairRow row = do
        dex <- dexOfRow row
        moveName <- moveNameOfRow row
        matchedMoveId <- requireName row "move_name" moveNameToId moveName
        pure (DexNumber dex, matchedMoveId)
  results <- traverse parseNamePairRow (csvRows csv)
  pure $ Map.fromListWith Set.union
    [ (dexNumber, Set.singleton matchedMoveId)
    | (dexNumber, matchedMoveId) <- results
    ]


-- | items.csv → Map itemId name
buildItems :: Gen -> CSV -> Either LoadError (Map ItemId Text)
buildItems gen csv = do
  itemIdOfRow <- intColumn csv "id"
  nameOfRow   <- textColumn csv "name"
  matching    <- forGen gen csv
  let parseItemRow row = do
        itemIdValue <- itemIdOfRow row
        name <- nameOfRow row
        pure (ItemId itemIdValue, name)
  pairs <- traverse parseItemRow matching
  pure $ Map.fromList pairs


-- | Raw CSV fields for one evolution row, before parsing into an EvoTrigger.
data RawEvolution = RawEvolution
  { rawMethod :: !Text
  , rawParam1 :: !(Maybe Text)
  , rawParam2 :: !(Maybe Text)
  }

-- | evolutions.csv → [EvolutionStep]
-- New schema: gen,from_dex,to_dex,method,param1,param2
-- Methods and params use pret ASM constant names.
buildEvolutions :: Gen -> CSV -> Either LoadError [EvolutionStep]
buildEvolutions gen csv = do
  fromDexOfRow <- intColumn csv "from_dex"
  toDexOfRow   <- intColumn csv "to_dex"
  methodOfRow  <- textColumn csv "method"
  param1OfRow  <- maybeTextColumn csv "param1"
  param2OfRow  <- maybeTextColumn csv "param2"
  matching     <- forGen gen csv
  let parseEvolutionRow row = do
        fromDex    <- fromDexOfRow row
        toDex      <- toDexOfRow row
        methodText <- methodOfRow row
        param1     <- param1OfRow row
        param2     <- param2OfRow row
        let raw = RawEvolution
              { rawMethod = methodText
              , rawParam1 = param1
              , rawParam2 = param2
              }
        trigger <- case parseTrigger raw of
          Right value  -> Right value
          Left message -> Left (UnknownEnum (rowFilePath row) (rowNumber row) "evolution" message)
        pure EvolutionStep
          { stepFrom    = DexNumber fromDex
          , stepTo      = DexNumber toDex
          , stepTrigger = trigger
          }
  traverse parseEvolutionRow matching

-- | Parse an evolution trigger from ASM constant names.
-- EVOLVE_TRADE and EVOLVE_TRADE_ITEM are distinct in the CSV
-- (normalized at extraction time), so no heuristic is needed.
parseTrigger :: RawEvolution -> Either Text EvoTrigger
parseTrigger RawEvolution{rawMethod, rawParam1, rawParam2} =
  case rawMethod of
    "EVOLVE_LEVEL" -> do
      param <- requireParam "param1" rawParam1
      level <- parseIntField param
      pure $ EvoLevel (Level level)
    "EVOLVE_ITEM" -> do
      param <- requireParam "param1" rawParam1
      pure $ EvoItem param
    "EVOLVE_TRADE" -> pure EvoTrade
    "EVOLVE_TRADE_ITEM" -> do
      param <- requireParam "param1" rawParam1
      pure $ EvoTradeItem param
    "EVOLVE_HAPPINESS" -> do
      param <- requireParam "param1" rawParam1
      case param of
        "TR_ANYTIME" -> pure EvoHappiness
        "TR_MORNDAY" -> pure EvoHappinessDay
        "TR_NITE"    -> pure EvoHappinessNight
        unrecognized -> Left $ "unknown happiness time: " <> unrecognized
    "EVOLVE_STAT" -> do
      levelParam <- requireParam "param1" rawParam1
      compParam  <- requireParam "param2" rawParam2
      level <- parseIntField levelParam
      case compParam of
        "ATK_LT_DEF" -> pure $ EvoStatLT (Level level)
        "ATK_GT_DEF" -> pure $ EvoStatGT (Level level)
        "ATK_EQ_DEF" -> pure $ EvoStatEQ (Level level)
        unrecognized -> Left $ "unknown stat comparison: " <> unrecognized
    unrecognized -> Left $ "unknown evolution method: " <> unrecognized

requireParam :: Text -> Maybe Text -> Either Text Text
requireParam label Nothing      = Left $ "missing " <> label
requireParam _     (Just value) = Right value

parseIntField :: Text -> Either Text Int
parseIntField rawValue = case decimal rawValue of
  Right (parsed, remainder) | Text.null remainder -> Right parsed
  _ -> Left $ "unparseable integer: " <> rawValue


-- ── Validation ────────────────────────────────────────────────────

-- | Check that every dex number referenced in evolution steps
-- exists in the species map. Catches dangling references before
-- they cause silent Map.lookup failures in the legality engine.
validateEvolutionDexNumbers :: FilePath -> Map DexNumber Species -> [EvolutionStep] -> Either LoadError ()
validateEvolutionDexNumbers path speciesMap evolutions =
  traverse_ checkStep evolutions
  where
    checkStep step = do
      checkDex (stepFrom step)
      checkDex (stepTo step)
    checkDex dex
      | Map.member dex speciesMap = Right ()
      | otherwise = Left (DanglingEvolution path dex)


-- | internal_index.csv → Map InternalIndex DexNumber
-- No gen column — this is Gen 1 only.
buildInternalIndex :: CSV -> Either LoadError (Map InternalIndex DexNumber)
buildInternalIndex csv = do
  internalIndexOfRow <- intColumn csv "internal_index"
  dexOfRow           <- intColumn csv "dex"
  let parseRow row = do
        idx <- internalIndexOfRow row
        dex <- dexOfRow row
        pure (InternalIndex (fromIntegral idx), DexNumber dex)
  pairs <- traverse parseRow (csvRows csv)
  pure $ Map.fromList pairs


-- ── Flag Name Maps ──────────────────────────────────────────────

-- | Build Gen 1 event/toggle flag, map script, badge, gym leader,
-- town, and trade name mappings.
buildGen1FlagNames :: CSV -> CSV -> CSV -> CSV -> CSV -> CSV -> CSV -> CSV -> Either LoadError Gen1FlagNames
buildGen1FlagNames eventCsv toggleCsv mapCsv badgeCsv gymLeaderCsv townCsv tradesRBCsv tradesYellowCsv = do
  eventFlags  <- buildIndexNameMap eventCsv "bit_index" "event_name"
  toggleFlags <- buildIndexNameMap toggleCsv "bit_index" "toggle_name"
  mapScripts  <- buildIndexNameMap mapCsv "byte_offset" "script_name"
  badges      <- buildIndexNameMap badgeCsv "bit_index" "badge_name"
  gymLeaders  <- buildIndexNameMap gymLeaderCsv "bit_index" "leader_name"
  towns       <- buildIndexNameMap townCsv "bit_index" "town_name"
  tradesRB    <- buildTradeNames tradesRBCsv
  tradesYellow <- buildTradeNames tradesYellowCsv
  pure Gen1FlagNames
    { eventFlagNames   = eventFlags
    , toggleFlagNames  = toggleFlags
    , mapScriptNames   = mapScripts
    , badgeNames       = badges
    , gymLeaderNames   = gymLeaders
    , townNames        = towns
    , tradeNamesRB     = tradesRB
    , tradeNamesYellow = tradesYellow
    }

-- | Generic loader for index→name CSVs (no gen column).
buildIndexNameMap :: CSV -> ColumnName -> ColumnName -> Either LoadError (Map Int Text)
buildIndexNameMap csv indexColumnName nameColumnName = do
  indexOfRow <- intColumn csv indexColumnName
  nameOfRow  <- textColumn csv nameColumnName
  let parseRow row = do
        idx  <- indexOfRow row
        name <- nameOfRow row
        pure (idx, name)
  pairs <- traverse parseRow (csvRows csv)
  pure $ Map.fromList pairs

-- | Load a trade CSV and format each row as a display label:
-- "GIVE_SPECIES \x2192 RECEIVE_SPECIES (NICKNAME)"
buildTradeNames :: CSV -> Either LoadError (Map Int Text)
buildTradeNames csv = do
  indexOfRow    <- intColumn csv "bit_index"
  giveOfRow     <- textColumn csv "give_species"
  receiveOfRow  <- textColumn csv "receive_species"
  nicknameOfRow <- textColumn csv "nickname"
  let parseRow row = do
        idx      <- indexOfRow row
        give     <- giveOfRow row
        receive  <- receiveOfRow row
        nickname <- nicknameOfRow row
        pure (idx, give <> " \x2192 " <> receive <> " (" <> nickname <> ")")
  pairs <- traverse parseRow (csvRows csv)
  pure $ Map.fromList pairs


-- | Check for cycles in the evolvesFrom map. A cycle means some
-- species can reach itself by walking backward through evolution
-- steps, which indicates corrupt evolution data.
detectEvolutionCycle :: FilePath -> Map DexNumber [EvolutionStep] -> Either LoadError ()
detectEvolutionCycle path evolvesFrom =
  traverse_ (walkAncestors Set.empty) (Map.keys evolvesFrom)
  where
    walkAncestors visited dex
      | Set.member dex visited = Left (EvolutionCycle path dex)
      | otherwise =
          case Map.lookup dex evolvesFrom of
            Nothing    -> Right ()
            Just steps ->
              traverse_ (walkAncestors (Set.insert dex visited) . stepFrom) steps
