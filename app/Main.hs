{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Pokemon.Types
import Pokemon.Data (loadGameData)
import Pokemon.Stats
import Pokemon.Legality (classifyMove)
import Pokemon.TextCodec
  (TextCodec (..), NamingScreen (..), loadCodec, encodeText, decodeText,
   displayText, showHexByte, lookupChar, lookupLigature)


main :: IO ()
main = do
  putStrLn "Loading game data..."
  gen1Data <- loadGameData Gen1
  gen2Data <- loadGameData Gen2
  putStrLn $ "  Gen 1: " ++ show (Map.size (gameSpecies gen1Data)) ++ " species, "
           ++ show (Map.size (gameMoves gen1Data)) ++ " moves, "
           ++ show (Map.size (gameMachines gen1Data)) ++ " TM/HMs"
  putStrLn $ "  Gen 2: " ++ show (Map.size (gameSpecies gen2Data)) ++ " species, "
           ++ show (Map.size (gameMoves gen2Data)) ++ " moves, "
           ++ show (Map.size (gameMachines gen2Data)) ++ " TM/HMs"
  putStrLn $ "         " ++ show (Map.size (gameEggMoves gen2Data)) ++ " species w/ egg moves, "
           ++ show (Map.size (gameTutorMoves gen2Data)) ++ " species w/ tutor moves"

  section "Stat Calculation"
  demoStats gen1Data gen2Data

  section "Move Legality"
  demoLegality gen1Data gen2Data

  section "Text Codec"
  demoTextCodec


-- ── Helpers ─────────────────────────────────────────────────────

section :: String -> IO ()
section title = putStrLn $
  "\x2500\x2500 " ++ title ++ " " ++ replicate (50 - 4 - length title) '\x2500'

-- | Find a species by name in a GameData.
findSpecies :: GameData -> T.Text -> Maybe (DexNumber, Species)
findSpecies gameData name = do
  dex <- Map.lookup name (gameSpeciesByName gameData)
  species <- Map.lookup dex (gameSpecies gameData)
  pure (dex, species)

-- | Find a move by name in a GameData.
findMove :: GameData -> T.Text -> Maybe (MoveId, Move)
findMove gameData name = do
  matchedMoveId <- Map.lookup name (gameMoveByName gameData)
  move <- Map.lookup matchedMoveId (gameMoves gameData)
  pure (matchedMoveId, move)


-- ── Stat Calculation Demo ───────────────────────────────────────

demoStats :: GameData -> GameData -> IO ()
demoStats gen1Data gen2Data = do
  case Map.lookup (DexNumber 25) (gameSpecies gen1Data) of
    Nothing -> putStrLn "  Pikachu not found in Gen 1!"
    Just pikachu -> do
      let statsNoExp  = calcAllStats pikachu maxDVs zeroStatExp (Level 50)
          statsMaxExp = calcAllStats pikachu maxDVs maxStatExp (Level 50)
          growthRate  = speciesGrowthRate pikachu
      putStrLn "  Pikachu (Gen 1), level 50, max DVs:"
      putStrLn $ "    No stat exp:  " ++ showStats statsNoExp
      putStrLn $ "    Max stat exp: " ++ showStats statsMaxExp
      putStrLn $ "    Growth rate: " ++ show growthRate
               ++ " (" ++ show (expForLevel growthRate (Level 100)) ++ " exp to L100)"

  case Map.lookup (DexNumber 25) (gameSpecies gen2Data) of
    Nothing -> pure ()
    Just pikachu -> do
      let statsNoExp  = calcAllStats pikachu maxDVs zeroStatExp (Level 50)
          statsMaxExp = calcAllStats pikachu maxDVs maxStatExp (Level 50)
      putStrLn "  Pikachu (Gen 2), level 50, max DVs:"
      putStrLn $ "    No stat exp:  " ++ showStats statsNoExp
      putStrLn $ "    Max stat exp: " ++ showStats statsMaxExp

  -- Shiny DV check
  let shinyExample = DVs 10 10 10 10
      nonShiny     = DVs 15 15 15 15
  putStrLn $ "  DVs " ++ showDVs shinyExample ++ ": shiny = " ++ show (isShiny shinyExample)
  putStrLn $ "  DVs " ++ showDVs nonShiny     ++ ": shiny = " ++ show (isShiny nonShiny)


showStats :: CalcStats -> String
showStats stats =
  "HP " ++ show (statHP stats)
  ++ "  Atk " ++ show (statAttack stats)
  ++ "  Def " ++ show (statDefense stats)
  ++ "  Spd " ++ show (statSpeed stats)
  ++ "  " ++ showSpecial (statSpecial stats)

showSpecial :: Special -> String
showSpecial (Unified specialValue)       = "Spc " ++ show specialValue
showSpecial (Split spAttack spDefense) = "SpA " ++ show spAttack ++ "  SpD " ++ show spDefense

showDVs :: DVs -> String
showDVs dvs = "{Atk=" ++ show (dvAttack dvs) ++ " Def=" ++ show (dvDefense dvs)
           ++ " Spd=" ++ show (dvSpeed dvs) ++ " Spc=" ++ show (dvSpecial dvs) ++ "}"


-- ── Move Legality Demo ──────────────────────────────────────────

demoLegality :: GameData -> GameData -> IO ()
demoLegality gen1Data gen2Data = do
  -- Simple: direct level-up + TM in same gen
  classify gen2Data (Just gen1Data) "PIKACHU"  "THUNDERBOLT"  (Level 100)

  -- PreEvo: Raichu can't learn Thunder Wave in Gen 2, but Pikachu can
  classify gen2Data (Just gen1Data) "RAICHU"   "THUNDER_WAVE" (Level 100)

  -- Tradeback: Mega Punch is TM01 in Gen 1, gone in Gen 2
  classify gen2Data (Just gen1Data) "CHANSEY"  "MEGA_PUNCH"   (Level 100)

  -- Tradeback + tutor: Body Slam is TM08 in Gen 1, tutor in Crystal
  classify gen2Data (Just gen1Data) "SNORLAX"  "BODY_SLAM"    (Level 100)

  -- Reverse tradeback: Eevee learns Bite in Gen 2 but not Gen 1
  classify gen1Data (Just gen2Data) "EEVEE"    "BITE"         (Level 100)

  -- Level-restricted: Pikachu at level 10 can't have Thunderbolt yet
  classify gen2Data (Just gen1Data) "PIKACHU"  "THUNDERBOLT"  (Level 10)


classify :: GameData -> Maybe GameData -> T.Text -> T.Text -> Level -> IO ()
classify gameData otherGameData targetSpecies targetMove level =
  case (findSpecies gameData targetSpecies, findMove gameData targetMove) of
    (Nothing, _) -> TIO.putStrLn $ "  " <> targetSpecies <> ": species not found"
    (_, Nothing) -> TIO.putStrLn $ "  " <> targetMove <> ": move not found"
    (Just (dex, _), Just (targetMoveId, _)) -> do
      let genLabel = case gameGen gameData of Gen1 -> "Gen 1"; Gen2 -> "Gen 2"
          sources = classifyMove gameData otherGameData dex targetMoveId level
      TIO.putStrLn $ "  " <> targetSpecies <> " + " <> targetMove
                   <> " (" <> genLabel <> ", L" <> T.pack (show (unLevel level)) <> "):"
      if null sources
        then putStrLn "    (not learnable)"
        else putStr $ renderSources "    " sources


-- | Render a list of LearnSources as a box-drawing tree.
renderSources :: String -> [LearnSource] -> String
renderSources prefix sources = unlines (renderBranches prefix sources)
  where
    renderBranches _ []               = []
    renderBranches indent [source]     = renderOne indent "\x2514\x2500\x2500 " "    " source
    renderBranches indent (source:rest) = renderOne indent "\x251C\x2500\x2500 " "\x2502   " source ++ renderBranches indent rest

    renderOne indent branch cont source =
      (indent ++ branch ++ showSource source)
      : renderBranches (indent ++ cont) (sourceVia source)

    showSource source =
      methodLabel (sourceMethod source) ++ " (" ++ T.unpack (sourceDetail source) ++ ")"


-- | Human-readable label for a LearnMethod.
methodLabel :: LearnMethod -> String
methodLabel LevelUp    = "Level-up"
methodLabel TMMachine  = "TM"
methodLabel HMMachine  = "HM"
methodLabel EggMove    = "Egg move"
methodLabel TutorMove  = "Tutor"
methodLabel Tradeback  = "Tradeback"
methodLabel EventMove  = "Event"
methodLabel PreEvo     = "Pre-evo"


-- ── Text Codec Demo ─────────────────────────────────────────────

demoTextCodec :: IO ()
demoTextCodec = do
  (codec, screens) <- loadCodec Gen1 English
  putStrLn $ "  Gen 1 English codec: "
           ++ show (Map.size (codecDecode codec)) ++ " mapped characters"

  -- Encode a name to Game Boy bytes, then decode it back
  let encodeChar char = case lookupChar codec char of
        Just gameChar -> gameChar
        Nothing       -> error $ "Character not in codec: " ++ [char]
      name = GameText (map encodeChar "PIKACHU")
      encoded = encodeText 11 name
      decoded = decodeText codec encoded
  putStrLn $ "  Encode \"PIKACHU\"  \x2192 " ++ showHexBytes encoded
  TIO.putStrLn $ "  Decode back      \x2192 " <> displayText decoded

  -- Ligature round-trip (the PK and MN symbols from the games)
  let encodeLigature text = case lookupLigature codec text of
        Just gameChar -> gameChar
        Nothing       -> error $ "Ligature not in codec: " ++ T.unpack text
      ligatureText    = GameText [encodeLigature "PK", encodeLigature "MN"]
      ligatureEncoded = encodeText 11 ligatureText
      ligatureDecoded = decodeText codec ligatureEncoded
  putStrLn $ "  Encode [PK][MN]  \x2192 " ++ showHexBytes ligatureEncoded
  TIO.putStrLn $ "  Decode back      \x2192 " <> displayText ligatureDecoded

  -- Naming screen info
  putStrLn $ "  Naming screens: " ++ show (length screens)
  mapM_ showScreen screens


showScreen :: NamingScreen -> IO ()
showScreen screen = TIO.putStrLn $
  "    " <> screenLabel screen <> ": "
  <> T.pack (show (Set.size (screenChars screen))) <> " choosable characters"


-- | Show a ByteString as space-separated hex pairs.
showHexBytes :: BS.ByteString -> String
showHexBytes bytes = unwords [ showHexByte byte | byte <- BS.unpack bytes ]
