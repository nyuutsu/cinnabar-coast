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

-- | Read a CSV file, drop the header, split each line on commas.
-- Blank lines are silently skipped.
readCSV :: FilePath -> IO [[T.Text]]
readCSV path = do
  content <- T.readFile path
  pure [ map T.strip (T.splitOn "," line)
       | line <- drop 1 (T.lines content)
       , not (T.null (T.strip line))
       ]


-- ── Field parsers ───────────────────────────────────────────────

-- | Parse a Text field as Int, or 0 if empty/unparseable.
int :: T.Text -> Int
int t
  | T.null (T.strip t) = 0
  | otherwise = case reads (T.unpack t) of
      [(n, "")] -> n
      _         -> 0

-- | Parse a Text field as Maybe Int. Empty → Nothing.
maybeInt :: T.Text -> Maybe Int
maybeInt t
  | T.null (T.strip t) = Nothing
  | otherwise = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Filter CSV rows where the first column matches a gen number.
forGen :: Gen -> [[T.Text]] -> [[T.Text]]
forGen gen = filter match
  where
    genN = fromEnum gen + 1
    match (g:_) = int g == genN
    match []    = False


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
-- Schema: gen,index,dex,name,hp,atk,dfn,spd,spc,spa,sp_def,
--         type1,type2,catch_rate,gender_ratio,egg_group1,egg_group2,
--         base_happiness,growth_rate
loadSpecies :: Gen -> FilePath -> IO (Map.Map Int Species)
loadSpecies gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
  pure $ Map.fromList
    [ (speciesDex sp, sp)
    | row <- matching
    , let sp = parseSpecies gen row
    ]

parseSpecies :: Gen -> [T.Text] -> Species
parseSpecies gen row = Species
  { speciesDex           = int (row !! 2)
  , speciesName          = T.strip (row !! 3)
  , speciesBaseStats     = BaseStats
      { baseHP      = int (row !! 4)
      , baseAttack  = int (row !! 5)
      , baseDefense = int (row !! 6)
      , baseSpeed   = int (row !! 7)
      , baseSpecial = case gen of
          Gen1 -> Unified (int (row !! 8))
          Gen2 -> Split   (int (row !! 9)) (int (row !! 10))
      }
  , speciesTypes         = (typeFromId (int (row !! 11)), typeFromId (int (row !! 12)))
  , speciesCatchRate     = int (row !! 13)
  , speciesGrowthRate    = growthFromId (int (row !! 18))
  , speciesGenderRatio   = maybeInt (row !! 14)
  , speciesEggGroups     = case (maybeInt (row !! 15), maybeInt (row !! 16)) of
      (Just a, Just b) -> Just (a, b)
      _                -> Nothing
  , speciesBaseHappiness = maybeInt (row !! 17)
  }


-- | moves.csv → Map moveId Move
-- Schema: gen,id,name,type,power,accuracy,pp
loadMoves :: Gen -> FilePath -> IO (Map.Map Int Move)
loadMoves gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
  pure $ Map.fromList
    [ (moveId mv, mv)
    | row <- matching
    , let mv = Move
            { moveId       = int (row !! 1)
            , moveName     = T.strip (row !! 2)
            , moveType     = typeFromId (int (row !! 3))
            , movePower    = int (row !! 4)
            , moveAccuracy = int (row !! 5)
            , movePP       = int (row !! 6)
            }
    ]


-- | tmhm.csv → Map Machine moveId
-- Schema: gen,number,move_id,is_hm
loadMachines :: Gen -> FilePath -> IO (Map.Map Machine Int)
loadMachines gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
  pure $ Map.fromList
    [ (machine, int (row !! 2))
    | row <- matching
    , let num   = int (row !! 1)
          isHM  = int (row !! 3) == 1
          machine = if isHM then HM num else TM num
    ]


-- | tmhm_compat.csv → Map dex (Set Machine)
-- Schema: gen,dex,number
-- Numbers 1–50 are TMs; 51+ are HMs (51=HM01, 52=HM02, etc.)
loadCompat :: Gen -> FilePath -> IO (Map.Map Int (Set.Set Machine))
loadCompat gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
      pairs = [ (int (row !! 1), numToMachine (int (row !! 2)))
              | row <- matching
              ]
  pure $ Map.fromListWith Set.union
    [ (dex, Set.singleton m)
    | (dex, m) <- pairs
    ]

numToMachine :: Int -> Machine
numToMachine n
  | n <= 50   = TM n
  | otherwise = HM (n - 50)


-- | learnsets.csv → Map dex [(level, moveId)]
-- Schema: gen,dex,level,move_id
loadLearnsets :: Gen -> FilePath -> IO (Map.Map Int [(Int, Int)])
loadLearnsets gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
      triples = [ (int (row !! 1), (int (row !! 2), int (row !! 3)))
                | row <- matching
                ]
  pure $ Map.fromListWith (++)
    [ (dex, [entry])
    | (dex, entry) <- triples
    ]


-- | egg_moves.csv or tutor.csv → Map dex (Set moveId)
-- Schema: dex,move_id (no gen column — these are Gen 2 only)
loadPairMap :: FilePath -> IO (Map.Map Int (Set.Set Int))
loadPairMap path = do
  rows <- readCSV path
  pure $ Map.fromListWith Set.union
    [ (int (row !! 0), Set.singleton (int (row !! 1)))
    | row <- rows
    ]


-- | items.csv → Map itemId name
-- Schema: gen,id,name
loadItems :: FilePath -> IO (Map.Map Int T.Text)
loadItems path = do
  rows <- readCSV path
  pure $ Map.fromList
    [ (int (row !! 1), T.strip (row !! 2))
    | row <- rows
    , int (row !! 0) == 2
    ]


-- | evolutions.csv → [EvolutionStep]
-- Schema: gen,from_dex,to_dex,method,param
loadEvolutions :: Gen -> FilePath -> IO [EvolutionStep]
loadEvolutions gen path = do
  rows <- readCSV path
  let matching = forGen gen rows
  pure [ EvolutionStep
           { stepFrom    = int (row !! 1)
           , stepTo      = int (row !! 2)
           , stepTrigger = parseTrigger (row !! 3) (row !! 4)
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
