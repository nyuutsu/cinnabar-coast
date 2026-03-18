{-# LANGUAGE OverloadedStrings #-}

-- | Extraction orchestrator: reads pokered and pokecrystal ASM sources,
-- produces CSV files in data/csv/.
--
-- Usage: extract <pokered-path> <pokecrystal-path>

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName)
import Text.Megaparsec

import Extract.ASM
import Extract.Moves (extractMoves, movesHeader)
import Extract.TMHM (extractTMHM, tmhmHeader)
import Extract.Items (extractItems, itemsHeader)
import Extract.EggMoves (extractEggMoves, eggMovesHeader)
import Extract.EvosAttacks (parseEvosAttacksFile, formatLearnsetRows,
                            formatEvolutionRows, learnsetHeader,
                            evolutionHeader)
import Extract.Species (extractSpeciesFile, parseBaseStatsIncludes,
                        formatGen1Species, formatGen2Species,
                        gen1StartingMoves,
                        speciesHeader, tmhmCompatHeader, SpeciesData(..))


main :: IO ()
main = do
  args <- getArgs
  case args of
    [pokeredPath, pokecrystalPath] ->
      extractAll pokeredPath pokecrystalPath "data/csv"
    _ -> putStrLn "Usage: extract <pokered-path> <pokecrystal-path>"


extractAll :: FilePath -> FilePath -> FilePath -> IO ()
extractAll pokered pokecrystal outDir = do

  -- ── TM/HM data (needed for tmhm_compat) ─────────────────────
  gen1TMHM <- parseFile parseTMHMBlock (pokered </> "constants/item_constants.asm")
  gen2TMHM <- parseFile parseTMHMBlock (pokecrystal </> "constants/item_constants.asm")

  -- Build move_name → TM/HM number maps (1-based, TMs then HMs then tutors)
  let gen1MoveToNumber = buildMoveNumberMap gen1TMHM
      gen2MoveToNumber = buildMoveNumberMap gen2TMHM

  -- ── Gen 1 internal index → dex number mapping ───────────────
  (gen1DexOrder, gen1InternalToDex) <- parseDexOrder
    (pokered </> "constants/pokedex_constants.asm")
    (pokered </> "data/pokemon/dex_order.asm")
  gen1PokemonConsts <- parseFile parseConstBlock
    (pokered </> "constants/pokemon_constants.asm")

  -- Species constant name → dex number (for Gen 1 evolution targets)
  let gen1NameToDex = buildGen1NameToDex gen1PokemonConsts gen1InternalToDex

  -- ── Gen 2 species constants ──────────────────────────────────
  gen2PokemonConsts <- parseFile parseConstBlock
    (pokecrystal </> "constants/pokemon_constants.asm")

  -- ── Moves ────────────────────────────────────────────────────
  gen1Moves <- extractMoves "1" (pokered </> "data/moves/moves.asm")
  gen2Moves <- extractMoves "2" (pokecrystal </> "data/moves/moves.asm")
  writeCSV (outDir </> "moves.csv") movesHeader (gen1Moves ++ gen2Moves)

  -- ── TM/HM ────────────────────────────────────────────────────
  gen1MachineRows <- extractTMHM "1" (pokered </> "constants/item_constants.asm")
  gen2MachineRows <- extractTMHM "2" (pokecrystal </> "constants/item_constants.asm")
  writeCSV (outDir </> "tmhm.csv") tmhmHeader (gen1MachineRows ++ gen2MachineRows)

  -- ── Items ────────────────────────────────────────────────────
  gen1Items <- extractItems "1" (pokered </> "data/items/names.asm")
  gen2Items <- extractItems "2" (pokecrystal </> "data/items/names.asm")
  writeCSV (outDir </> "items.csv") itemsHeader (gen1Items ++ gen2Items)

  -- ── Species ──────────────────────────────────────────────────
  gen1SpeciesData <- extractAllSpecies pokered
  gen2SpeciesData <- extractAllSpecies pokecrystal

  let gen1SpeciesRows = [formatGen1Species name dex parsedSpecies
                           | (dex, name, parsedSpecies) <- gen1SpeciesData]
      gen2SpeciesRows = [formatGen2Species name dex parsedSpecies
                           | (dex, name, parsedSpecies) <- gen2SpeciesData]
  writeCSV (outDir </> "species.csv") speciesHeader
    (gen1SpeciesRows ++ gen2SpeciesRows)

  -- ── TM/HM compatibility ─────────────────────────────────────
  let gen1Compat = buildTMHMCompat "1" gen1MoveToNumber gen1SpeciesData
      gen2Compat = buildTMHMCompat "2" gen2MoveToNumber gen2SpeciesData
  writeCSV (outDir </> "tmhm_compat.csv") tmhmCompatHeader
    (gen1Compat ++ gen2Compat)

  -- ── Tutor compatibility (Gen 2 only) ─────────────────────────
  let tutorCompat = buildTutorCompat gen2TMHM gen2SpeciesData
  writeCSV (outDir </> "tutor.csv") ["dex", "move_name"] tutorCompat

  -- ── Egg moves (Gen 2 only) ──────────────────────────────────
  eggMoves <- extractEggMoves
    (pokecrystal </> "data/pokemon/egg_move_pointers.asm")
    (pokecrystal </> "data/pokemon/egg_moves.asm")
  writeCSV (outDir </> "egg_moves.csv") eggMovesHeader eggMoves

  -- ── Evolutions & Learnsets ───────────────────────────────────
  -- Gen 1: blocks in internal index order, need mapping to dex
  gen1EvosAttacks <- parseFile parseEvosAttacksFile
    (pokered </> "data/pokemon/evos_moves.asm")
  let gen1EvosAttacksJoined = joinGen1Blocks gen1DexOrder gen1EvosAttacks

  -- Gen 2: blocks in dex order
  gen2EvosAttacks <- parseFile parseEvosAttacksFile
    (pokecrystal </> "data/pokemon/evos_attacks.asm")
  let gen2EvosAttacksJoined = zip [1 :: Int ..] (map snd gen2EvosAttacks)

  -- Gen 1 starting moves live in base_stats, not evos_moves.
  -- Gen 2 has them inline in evos_attacks.asm (as level 1 entries).
  let gen1StartingRows =
        [ ["1", T.pack (show dex), "1", move]
        | (dex, _name, parsedSpecies) <- gen1SpeciesData
        , move <- gen1StartingMoves parsedSpecies
        ]
  let learnsetRows = gen1StartingRows
                   ++ formatLearnsetRows "1" gen1EvosAttacksJoined
                   ++ formatLearnsetRows "2" gen2EvosAttacksJoined
  writeCSV (outDir </> "learnsets.csv") learnsetHeader learnsetRows

  let evolutionRows = formatEvolutionRows "1" gen1NameToDex gen1EvosAttacksJoined
                    ++ formatEvolutionRows "2" gen2PokemonConsts gen2EvosAttacksJoined
  writeCSV (outDir </> "evolutions.csv") evolutionHeader evolutionRows

  -- ── Summary ──────────────────────────────────────────────────
  putStrLn "Extraction complete:"
  putStrLn $ "  moves:       " ++ show (length gen1Moves + length gen2Moves)
  putStrLn $ "  tmhm:        " ++ show (length gen1MachineRows + length gen2MachineRows)
  putStrLn $ "  items:       " ++ show (length gen1Items + length gen2Items)
  putStrLn $ "  species:     " ++ show (length gen1SpeciesRows + length gen2SpeciesRows)
  putStrLn $ "  tmhm_compat: " ++ show (length gen1Compat + length gen2Compat)
  putStrLn $ "  tutor:       " ++ show (length tutorCompat)
  putStrLn $ "  egg_moves:   " ++ show (length eggMoves)
  putStrLn $ "  learnsets:   " ++ show (length learnsetRows)
  putStrLn $ "  evolutions:  " ++ show (length evolutionRows)


-- ── Species extraction ─────────────────────────────────────────

-- | Extract all species from one gen's base_stats directory.
-- Parses the INCLUDE list for dex order, then each species file.
-- Returns (dex, name, data) triples. Name is derived from the
-- INCLUDE filename (e.g. "bulbasaur.asm" → "BULBASAUR").
-- Handles Mew specially (not in pokered's INCLUDE list).
extractAllSpecies :: FilePath -> IO [(Int, Text, SpeciesData)]
extractAllSpecies repoPath = do
  includes <- parseFile parseBaseStatsIncludes
    (repoPath </> "data/pokemon/base_stats.asm")
  speciesList <- mapM (\includePath -> extractSpeciesFile (repoPath </> T.unpack includePath))
    includes
  let nameFromInclude includePath = T.toUpper . T.pack $ takeBaseName (T.unpack includePath)
      names = map nameFromInclude includes
      numbered = zip3 [1 :: Int ..] names speciesList
      nextDex = length includes + 1

  -- Check for Mew (pokered excludes it from the INCLUDE list)
  mewExists <- doesFileExist (repoPath </> "data/pokemon/base_stats/mew.asm")
  if mewExists && nextDex == 151
    then do
      mewData <- extractSpeciesFile (repoPath </> "data/pokemon/base_stats/mew.asm")
      pure (numbered ++ [(151, "MEW", mewData)])
    else pure numbered


-- ── Gen 1 dex mapping ──────────────────────────────────────────

-- | Parse dex_order.asm to get internal index → dex number mapping.
-- Returns both forms: a list (for joinGen1Blocks, which zips it with
-- blocks in internal index order) and an IntMap (for buildGen1NameToDex,
-- which needs safe lookup by internal index).
parseDexOrder :: FilePath -> FilePath -> IO ([Maybe Int], IntMap Int)
parseDexOrder pokedexConstsPath dexOrderPath = do
  pokedexConsts <- parseFile parseConstBlock pokedexConstsPath
  dexNames <- parseFile parseDexOrderFile dexOrderPath
  let dexOrder = [ if name == "0"
                   then Nothing
                   else Map.lookup name pokedexConsts
                 | name <- dexNames
                 ]
      internalToDex = IntMap.fromList
        [ (idx, dex)
        | (idx, Just dex) <- zip [1..] dexOrder
        ]
  pure (dexOrder, internalToDex)

-- | Parse dex_order.asm: a table of `db DEX_NAME` or `db 0` lines.
parseDexOrderFile :: Parser [Text]
parseDexOrderFile = scanLines parseDbEntry
  where
    parseDbEntry = do
      _ <- keyword "db"
      arg <- takeWhile1P (Just "argument") (\char -> char /= '\n' && char /= ';' && char /= ' ' && char /= '\t')
      _ <- takeWhileP Nothing (/= '\n')
      endOfLine
      pure (T.strip arg)

-- | Build species constant name → dex number for Gen 1.
-- Composes: name → internal index → dex number.
buildGen1NameToDex :: Map Text Int -> IntMap Int -> Map Text Int
buildGen1NameToDex pokemonConsts internalToDex =
  Map.mapMaybe (\internalIdx -> IntMap.lookup internalIdx internalToDex) pokemonConsts

-- | Join Gen 1 evos/attacks blocks with dex numbers.
-- Blocks are in internal index order; we use the dex mapping to
-- filter out MissingNo and assign correct dex numbers.
joinGen1Blocks :: [Maybe Int] -> [(Text, a)] -> [(Int, a)]
joinGen1Blocks dexOrder blocks =
  [ (dex, blockData)
  | (Just dex, (_, blockData)) <- zip dexOrder blocks
  ]


-- ── TM/HM compatibility ───────────────────────────────────────

-- | Build a map from move name → unified TM/HM number.
-- TMs are numbered 1..N, HMs continue as N+1..N+M.
buildMoveNumberMap :: TMHM -> Map Text Int
buildMoveNumberMap tmhm =
  Map.fromList $
    zip (tmMoves tmhm) [1..] ++
    zip (hmMoves tmhm) [length (tmMoves tmhm) + 1 ..]

-- | Generate tmhm_compat rows from species data.
-- For each species' tmhm moves, look up the TM/HM number.
buildTMHMCompat :: Text -> Map Text Int -> [(Int, Text, SpeciesData)] -> [[Text]]
buildTMHMCompat gen moveToNumber speciesData =
  [ [gen, T.pack (show dex), T.pack (show number)]
  | (dex, _name, parsedSpecies) <- speciesData
  , moveName <- speciesTmhm parsedSpecies
  , Just number <- [Map.lookup moveName moveToNumber]
  ]

-- | Generate tutor compatibility rows.
-- Tutor moves are listed in the species' tmhm line but aren't TMs or HMs.
buildTutorCompat :: TMHM -> [(Int, Text, SpeciesData)] -> [[Text]]
buildTutorCompat tmhm speciesData =
  let tutorSet = Set.fromList (tutorMoves tmhm)
  in [ [T.pack (show dex), moveName]
     | (dex, _name, parsedSpecies) <- speciesData
     , moveName <- speciesTmhm parsedSpecies
     , Set.member moveName tutorSet
     ]


-- ── CSV output ─────────────────────────────────────────────────

writeCSV :: FilePath -> [Text] -> [[Text]] -> IO ()
writeCSV path header rows = do
  let headerLine = T.intercalate "," header
      dataLines  = map (T.intercalate ",") rows
  TIO.writeFile path (T.unlines (headerLine : dataLines))
  putStrLn $ "  Wrote " ++ path ++ " (" ++ show (length rows) ++ " rows)"
