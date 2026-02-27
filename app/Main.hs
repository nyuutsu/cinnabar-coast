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


main :: IO ()
main = do
  putStrLn "Loading game data..."
  g1 <- loadGameData Gen1
  g2 <- loadGameData Gen2
  putStrLn $ "  Gen 1: " ++ show (Map.size (gameSpecies g1)) ++ " species, "
           ++ show (Map.size (gameMoves g1)) ++ " moves, "
           ++ show (Map.size (gameMachines g1)) ++ " TM/HMs"
  putStrLn $ "  Gen 2: " ++ show (Map.size (gameSpecies g2)) ++ " species, "
           ++ show (Map.size (gameMoves g2)) ++ " moves, "
           ++ show (Map.size (gameMachines g2)) ++ " TM/HMs"
  putStrLn $ "         " ++ show (Map.size (gameEggMoves g2)) ++ " species w/ egg moves, "
           ++ show (Map.size (gameTutorMoves g2)) ++ " species w/ tutor moves"

  section "Stat Calculation"
  demoStats g1 g2

  section "Move Legality"
  demoLegality g1 g2

  section "Text Codec"
  demoTextCodec


-- ── Helpers ─────────────────────────────────────────────────────

section :: String -> IO ()
section title = putStrLn $
  "\n\x2500\x2500 " ++ title ++ " " ++ replicate (50 - 4 - length title) '\x2500'

-- | Find a species by name in a GameData.
findSpecies :: GameData -> T.Text -> Maybe (Int, Species)
findSpecies gd name =
  case [ (dex, sp) | (dex, sp) <- Map.toList (gameSpecies gd)
                   , speciesName sp == name ] of
    (x:_) -> Just x
    []    -> Nothing

-- | Find a move by name in a GameData.
findMove :: GameData -> T.Text -> Maybe (Int, Move)
findMove gd name =
  case [ (mid, m) | (mid, m) <- Map.toList (gameMoves gd)
                  , moveName m == name ] of
    (x:_) -> Just x
    []    -> Nothing


-- ── Stat Calculation Demo ───────────────────────────────────────

demoStats :: GameData -> GameData -> IO ()
demoStats g1 g2 = do
  case Map.lookup 25 (gameSpecies g1) of
    Nothing -> putStrLn "  Pikachu not found in Gen 1!"
    Just pika -> do
      let stats0 = calcAllStats pika maxDVs zeroStatExp 50
          statsM = calcAllStats pika maxDVs maxStatExp 50
          rate   = speciesGrowthRate pika
      putStrLn "  Pikachu (Gen 1), level 50, max DVs:"
      putStrLn $ "    No stat exp:  " ++ showStats stats0
      putStrLn $ "    Max stat exp: " ++ showStats statsM
      putStrLn $ "    Growth rate: " ++ show rate
               ++ " (" ++ show (expForLevel rate 100) ++ " exp to L100)"

  case Map.lookup 25 (gameSpecies g2) of
    Nothing -> pure ()
    Just pika -> do
      let stats0 = calcAllStats pika maxDVs zeroStatExp 50
          statsM = calcAllStats pika maxDVs maxStatExp 50
      putStrLn "  Pikachu (Gen 2), level 50, max DVs:"
      putStrLn $ "    No stat exp:  " ++ showStats stats0
      putStrLn $ "    Max stat exp: " ++ showStats statsM

  -- Shiny DV check
  let shinyEx  = DVs 10 10 10 10
      nonShiny = DVs 15 15 15 15
  putStrLn $ "  DVs " ++ showDVs shinyEx  ++ ": shiny = " ++ show (isShiny shinyEx)
  putStrLn $ "  DVs " ++ showDVs nonShiny ++ ": shiny = " ++ show (isShiny nonShiny)


showStats :: CalcStats -> String
showStats s =
  "HP " ++ show (statHP s)
  ++ "  Atk " ++ show (statAttack s)
  ++ "  Def " ++ show (statDefense s)
  ++ "  Spd " ++ show (statSpeed s)
  ++ "  " ++ showSpecial (statSpecial s)

showSpecial :: Special -> String
showSpecial (Unified s)   = "Spc " ++ show s
showSpecial (Split sa sd) = "SpA " ++ show sa ++ "  SpD " ++ show sd

showDVs :: DVs -> String
showDVs d = "{Atk=" ++ show (dvAttack d) ++ " Def=" ++ show (dvDefense d)
         ++ " Spd=" ++ show (dvSpeed d) ++ " Spc=" ++ show (dvSpecial d) ++ "}"


-- ── Move Legality Demo ──────────────────────────────────────────

demoLegality :: GameData -> GameData -> IO ()
demoLegality g1 g2 = do
  -- Simple: direct level-up + TM in same gen
  classify g2 (Just g1) "PIKACHU"  "THUNDERBOLT"  100

  -- PreEvo: Raichu can't learn Thunder Wave in Gen 2, but Pikachu can
  classify g2 (Just g1) "RAICHU"   "THUNDER WAVE" 100

  -- Tradeback: Mega Punch is TM01 in Gen 1, gone in Gen 2
  classify g2 (Just g1) "CHANSEY"  "MEGA PUNCH"   100

  -- Tradeback + tutor: Body Slam is TM08 in Gen 1, tutor in Crystal
  classify g2 (Just g1) "SNORLAX"  "BODY SLAM"    100

  -- Reverse tradeback: Eevee learns Bite in Gen 2 but not Gen 1
  classify g1 (Just g2) "EEVEE"    "BITE"         100

  -- Level-restricted: Pikachu at level 10 can't have Thunderbolt yet
  classify g2 (Just g1) "PIKACHU"  "THUNDERBOLT"  10


classify :: GameData -> Maybe GameData -> T.Text -> T.Text -> Int -> IO ()
classify gd otherGd specName movName level =
  case (findSpecies gd specName, findMove gd movName) of
    (Nothing, _) -> TIO.putStrLn $ "  " <> specName <> ": species not found"
    (_, Nothing) -> TIO.putStrLn $ "  " <> movName <> ": move not found"
    (Just (dex, _), Just (mid, _)) -> do
      let genLabel = case gameGen gd of Gen1 -> "Gen 1"; Gen2 -> "Gen 2"
          sources = classifyMove gd otherGd dex mid level
      TIO.putStrLn $ "  " <> specName <> " + " <> movName
                   <> " (" <> genLabel <> ", L" <> T.pack (show level) <> "):"
      if null sources
        then putStrLn "    (not learnable)"
        else putStr $ renderSources "    " sources


-- | Render a list of LearnSources as a box-drawing tree.
renderSources :: String -> [LearnSource] -> String
renderSources prefix sources = unlines (go prefix sources)
  where
    go _ []          = []
    go pfx [src]     = renderOne pfx "\x2514\x2500\x2500 " "    " src
    go pfx (src:rest) = renderOne pfx "\x251C\x2500\x2500 " "\x2502   " src ++ go pfx rest

    renderOne pfx branch cont src =
      (pfx ++ branch ++ showSource src)
      : go (pfx ++ cont) (sourceVia src)

    showSource src =
      methodLabel (sourceMethod src) ++ " (" ++ T.unpack (sourceDetail src) ++ ")"


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
  let name = GameText [ Literal 'P', Literal 'I', Literal 'K', Literal 'A'
                       , Literal 'C', Literal 'H', Literal 'U' ]
      encoded = encodeText codec 11 name
      decoded = decodeText codec encoded
  putStrLn $ "  Encode \"PIKACHU\"  \x2192 " ++ showHexBytes encoded
  TIO.putStrLn $ "  Decode back      \x2192 " <> displayText decoded

  -- Ligature round-trip (the PK and MN symbols from the games)
  let pkText = GameText [Ligature "PK", Ligature "MN"]
      pkEnc  = encodeText codec 11 pkText
      pkDec  = decodeText codec pkEnc
  putStrLn $ "  Encode [PK][MN]  \x2192 " ++ showHexBytes pkEnc
  TIO.putStrLn $ "  Decode back      \x2192 " <> displayText pkDec

  -- Naming screen info
  putStrLn $ "  Naming screens: " ++ show (length screens)
  mapM_ showScreen screens


showScreen :: NamingScreen -> IO ()
showScreen ns = TIO.putStrLn $
  "    " <> screenLabel ns <> ": "
  <> T.pack (show (Set.size (screenChars ns))) <> " choosable characters"


-- | Show a ByteString as space-separated hex pairs.
showHexBytes :: BS.ByteString -> String
showHexBytes bs = unwords [ showHexByte b | b <- BS.unpack bs ]
