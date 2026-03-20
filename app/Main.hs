{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Cinnabar.Types
import Cinnabar.Data (loadGameData, loadAllGameData)
import Cinnabar.Error (loadOrDie)
import Cinnabar.Legality (classifyMove)
import Cinnabar.Save.Interpret
  ( interpretGen1Save, InterpretedSave (..), InterpretedMon (..)
  , InterpretedSpecies (..), InterpretedMove (..), SaveWarning (..)
  , InterpretedBox (..), InterpretedHoFEntry (..), InterpretedHoFRecord (..)
  , InventoryEntry (..), PlayTime (..)
  )
import Cinnabar.Save.Layout
  (cartridgeLayout, CartridgeLayout (..), GameVariant (..), SaveRegion (..))
import Cinnabar.Save.Raw
  (parseRawSave, SaveError (..), RawSaveFile (..), RawGen1SaveFile (..))
import Cinnabar.Stats
import Cinnabar.TextCodec
  (TextCodec (..), NamingScreen (..), loadCodec, encodeText, decodeText,
   displayText, showHexByte, lookupChar, lookupLigature)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["demo"]            -> runDemo
    ["read", savePath]  -> runReadCommand savePath
    ["boxes", savePath] -> runBoxesCommand savePath
    ["hof", savePath]   -> runHoFCommand savePath
    _                   -> usage


usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: cinnabar <command>"
  hPutStrLn stderr ""
  hPutStrLn stderr "Commands:"
  hPutStrLn stderr "  demo            Run demo output"
  hPutStrLn stderr "  read <file>     Read a Gen 1 save file"
  hPutStrLn stderr "  boxes <file>    Show PC box contents"
  hPutStrLn stderr "  hof <file>      Show Hall of Fame records"
  exitFailure


-- ── Save Loading ────────────────────────────────────────────────

loadGen1Save :: FilePath -> IO InterpretedSave
loadGen1Save savePath = do
  gen1Data <- loadOrDie =<< loadGameData Gen1
  (codec, _) <- loadOrDie =<< loadCodec Gen1 English

  readResult <- try (ByteString.readFile savePath)
  saveBytes <- case (readResult :: Either IOException ByteString) of
    Left readError -> do
      hPutStrLn stderr $ "Error reading file: " ++ show readError
      exitFailure
    Right bytes -> pure bytes

  layout <- case cartridgeLayout Yellow RegionWestern of
    Left errorMessage -> do
      hPutStrLn stderr $ "Layout error: " ++ Text.unpack errorMessage
      exitFailure
    Right cartLayout -> pure cartLayout

  rawSave <- case parseRawSave layout saveBytes of
    Left saveError -> do
      hPutStrLn stderr $ "Parse error: " ++ renderSaveError saveError
      exitFailure
    Right parsed -> pure parsed

  rawGen1 <- case rawSave of
    RawGen1Save raw -> pure raw
    _ -> do
      hPutStrLn stderr "Expected Gen 1 save file"
      exitFailure

  pure (interpretGen1Save gen1Data codec rawGen1)


-- ── Read Command ──────────────────────────────────────────────

runReadCommand :: FilePath -> IO ()
runReadCommand savePath = do
  interpreted <- loadGen1Save savePath
  printSaveSummary interpreted


-- ── Boxes Command ───────────────────────────────────────────────

runBoxesCommand :: FilePath -> IO ()
runBoxesCommand savePath = do
  interpreted <- loadGen1Save savePath
  printAllBoxes interpreted


-- ── HoF Command ──────────────────────────────────────────────

runHoFCommand :: FilePath -> IO ()
runHoFCommand savePath = do
  interpreted <- loadGen1Save savePath
  printHallOfFame interpreted


-- ── Save Display ──────────────────────────────────────────────

printSaveSummary :: InterpretedSave -> IO ()
printSaveSummary interpreted = do
  TextIO.putStrLn $ "Player: " <> displayText (interpPlayerName interpreted)
    <> " (ID: " <> Text.pack (show (unTrainerId (interpPlayerID interpreted))) <> ")"
  TextIO.putStrLn $ "Rival: " <> displayText (interpRivalName interpreted)
  let time = interpPlayTime interpreted
      timeStr = show (playHours time) ++ ":" ++ pad2 (playMinutes time)
                  ++ ":" ++ pad2 (playSeconds time)
      maxedStr = if interpPlayTimeMaxed interpreted then " (maxed)" else ""
  putStrLn $ "Time: " ++ timeStr ++ maxedStr
  putStrLn $ "Money: " ++ formatMoney (interpMoney interpreted)
  putStrLn $ "Coins: " ++ show (interpCasinoCoins interpreted)
  putStrLn ""

  case interpBadges interpreted of
    [] -> putStrLn "Badges: (none)"
    badges -> TextIO.putStrLn $ "Badges: " <> Text.intercalate ", " badges
  let ownedCount = Set.size (interpPokedexOwned interpreted)
      seenCount  = Set.size (interpPokedexSeen interpreted)
  putStrLn $ "Pokédex: " ++ show ownedCount ++ " owned, " ++ show seenCount ++ " seen"
  putStrLn ""

  let bagItems = interpBagItems interpreted
  case bagItems of
    [] -> pure ()
    _  -> do
      putStrLn $ "Bag (" ++ show (length bagItems) ++ " items):"
      mapM_ printInventoryEntry bagItems
      putStrLn ""

  let boxItems = interpBoxItems interpreted
  case boxItems of
    [] -> pure ()
    _  -> do
      putStrLn $ "PC Storage (" ++ show (length boxItems) ++ " items):"
      mapM_ printInventoryEntry boxItems
      putStrLn ""

  case interpPikachuFriend interpreted of
    Just friendship -> putStrLn $ "Pikachu Friendship: " ++ show friendship
    Nothing -> pure ()
  case interpDaycareSpecies interpreted of
    Just species -> putStrLn $ "Daycare: " ++ renderSpecies species
    Nothing -> pure ()
  putStrLn $ "Current Box: " ++ show (interpCurrentBox interpreted)
  let hofCount = interpHoFCount interpreted
  if hofCount == 0
    then putStrLn "Hall of Fame: (empty)"
    else putStrLn $ "Hall of Fame: " ++ show hofCount ++ " entries"
  putStrLn ""

  printBoxSummary interpreted
  putStrLn ""

  let party = interpParty interpreted
  putStrLn $ "Party (" ++ show (length party) ++ "):"
  mapM_ (uncurry printPartyMon) (zip [1 ..] party)
  case interpWarnings interpreted of
    [] -> pure ()
    warnings -> do
      putStrLn $ "Warnings (" ++ show (length warnings) ++ "):"
      mapM_ (putStrLn . ("  " ++) . renderWarning) warnings


printPartyMon :: Int -> InterpretedMon -> IO ()
printPartyMon slotNumber mon = do
  let speciesLabel  = renderSpecies (interpSpecies mon)
      levelValue    = unLevel (interpLevel mon)
      nicknameLabel = Text.unpack (displayText (interpNickname mon))
      moveLabels    = mapMaybe renderMove (interpMoves mon)
      dvs           = interpDVs mon
      shinyLabel    = if isShiny dvs then "shiny" else "not shiny"
  putStrLn $ "  " ++ show slotNumber ++ ". " ++ speciesLabel
    ++ " (Lv " ++ show levelValue ++ ") \"" ++ nicknameLabel ++ "\""
  putStrLn $ "     Moves: " ++ intercalate ", " moveLabels
  putStrLn $ "     DVs: Atk=" ++ show (dvAttack dvs)
    ++ " Def=" ++ show (dvDefense dvs)
    ++ " Spd=" ++ show (dvSpeed dvs)
    ++ " Spc=" ++ show (dvSpecial dvs)
    ++ " (HP=" ++ show (dvHP dvs) ++ ")"
    ++ " \xFF0F " ++ shinyLabel
  putStrLn $ "     HP: " ++ show (interpCurrentHP mon)
    ++ "/" ++ show (interpMaxHP mon)
  putStrLn ""


printBoxSummary :: InterpretedSave -> IO ()
printBoxSummary interpreted = do
  let activeBoxNum = interpActiveBoxNum interpreted
      boxCapacity = case interpRaw interpreted of
        RawGen1Save raw -> layoutBoxCapacity (rawGen1Layout raw)
        _ -> 20
      boxes = interpPCBoxes interpreted
      nonEmptyNums = Set.fromList (map interpBoxNumber boxes)
      displayNums = Set.toAscList (Set.insert activeBoxNum nonEmptyNums)
      boxCountMap = Map.fromList
        [(interpBoxNumber box, length (interpBoxMons box)) | box <- boxes]
  putStrLn "PC Boxes:"
  mapM_ (\boxNum -> do
    let count = Map.findWithDefault 0 boxNum boxCountMap
        activeLabel = if boxNum == activeBoxNum then " (active)" else ""
    if count == 0
      then putStrLn $ "  Box " ++ show boxNum ++ ": (empty)" ++ activeLabel
      else putStrLn $ "  Box " ++ show boxNum ++ ": " ++ show count
             ++ "/" ++ show boxCapacity ++ activeLabel
    ) displayNums


printAllBoxes :: InterpretedSave -> IO ()
printAllBoxes interpreted = do
  let boxes = interpPCBoxes interpreted
      boxCapacity = case interpRaw interpreted of
        RawGen1Save raw -> layoutBoxCapacity (rawGen1Layout raw)
        _ -> 20
  if null boxes
    then putStrLn "No Pokémon in PC boxes."
    else mapM_ (printBoxDetail boxCapacity) boxes


printBoxDetail :: Int -> InterpretedBox -> IO ()
printBoxDetail boxCapacity box = do
  let count = length (interpBoxMons box)
  putStrLn $ "Box " ++ show (interpBoxNumber box)
    ++ " (" ++ show count ++ "/" ++ show boxCapacity ++ "):"
  mapM_ (uncurry printPartyMon) (zip [1 ..] (interpBoxMons box))


printHallOfFame :: InterpretedSave -> IO ()
printHallOfFame interpreted = do
  let records = interpHallOfFame interpreted
  if null records
    then putStrLn "Hall of Fame: (empty)"
    else do
      putStrLn $ "Hall of Fame (" ++ show (length records) ++ " entries):"
      mapM_ (uncurry printHoFRecord) (zip [1 ..] records)

printHoFRecord :: Int -> InterpretedHoFRecord -> IO ()
printHoFRecord recordNumber record = do
  putStrLn ""
  putStrLn $ "  #" ++ show recordNumber ++ ":"
  case hofEntries record of
    [] -> putStrLn "    (empty)"
    entries -> mapM_ printHoFEntry entries

printHoFEntry :: InterpretedHoFEntry -> IO ()
printHoFEntry entry =
  let speciesLabel  = renderSpecies (hofSpecies entry)
      levelValue    = unLevel (hofLevel entry)
      nicknameLabel = Text.unpack (displayText (hofNickname entry))
  in putStrLn $ "    " ++ speciesLabel ++ " (Lv " ++ show levelValue
       ++ ") \"" ++ nicknameLabel ++ "\""


printInventoryEntry :: InventoryEntry -> IO ()
printInventoryEntry entry =
  TextIO.putStrLn $ "  " <> entryName entry <> " \xD7" <> Text.pack (show (entryQuantity entry))


formatMoney :: Int -> String
formatMoney amount = "$" ++ formatWithCommas amount

formatWithCommas :: Int -> String
formatWithCommas n
  | n < 1000  = show n
  | otherwise = formatWithCommas (n `div` 1000) ++ "," ++ padThree (n `mod` 1000)
  where
    padThree x = let s = show x in replicate (3 - length s) '0' ++ s

pad2 :: Int -> String
pad2 n
  | n < 10    = "0" ++ show n
  | otherwise = show n


renderSpecies :: InterpretedSpecies -> String
renderSpecies (KnownSpecies _ species) = Text.unpack (speciesName species)
renderSpecies (UnknownSpecies idx) =
  "Unknown [0x" ++ showHexByte (unInternalIndex idx) ++ "]"
renderSpecies (UnknownDexSpecies dex) =
  "Unknown [#" ++ show (unDex dex) ++ "]"


renderMove :: InterpretedMove -> Maybe String
renderMove EmptyMove          = Nothing
renderMove (KnownMove _ move) = Just (Text.unpack (moveName move))
renderMove (UnknownMove byte) = Just ("Unknown [0x" ++ showHexByte byte ++ "]")


renderSaveError :: SaveError -> String
renderSaveError (WrongFileSize expected actual) =
  "wrong file size: expected " ++ show expected ++ " bytes, got " ++ show actual
renderSaveError (UnsupportedLayout description) =
  "unsupported layout: " ++ Text.unpack description
renderSaveError (UnimplementedGen gen) =
  "unimplemented: " ++ show gen


-- | Render a save warning with 1-indexed slot numbers for display.
renderWarning :: SaveWarning -> String
renderWarning (UnknownSpeciesIndex slot idx) =
  "Slot " ++ show (slot + 1) ++ ": unknown species index 0x"
    ++ showHexByte (unInternalIndex idx)
renderWarning (UnknownMoveId slot moveSlot byte) =
  "Slot " ++ show (slot + 1) ++ ", move " ++ show moveSlot
    ++ ": unknown move ID 0x" ++ showHexByte byte
renderWarning (SpeciesListMismatch slot listByte structByte) =
  "Slot " ++ show (slot + 1) ++ ": species list/struct mismatch (0x"
    ++ showHexByte listByte ++ " vs 0x" ++ showHexByte structByte ++ ")"
renderWarning (ChecksumMismatch stored calculated) =
  "Checksum mismatch: stored 0x" ++ showHexByte stored
    ++ ", calculated 0x" ++ showHexByte calculated
renderWarning (StatMismatch slot statName stored calculated) =
  "Slot " ++ show (slot + 1) ++ ": " ++ Text.unpack statName
    ++ " mismatch (stored " ++ show stored
    ++ ", calculated " ++ show calculated ++ ")"
renderWarning (BoxBankChecksumMismatch bankIndex stored calculated) =
  "Box bank " ++ show bankIndex ++ ": checksum mismatch (stored 0x"
    ++ showHexByte stored ++ ", calculated 0x" ++ showHexByte calculated ++ ")"
renderWarning (BoxChecksumMismatch bankIndex boxIndex stored calculated) =
  "Box bank " ++ show bankIndex ++ ", box " ++ show boxIndex
    ++ ": checksum mismatch (stored 0x" ++ showHexByte stored
    ++ ", calculated 0x" ++ showHexByte calculated ++ ")"


-- ── Demo Command ──────────────────────────────────────────────

runDemo :: IO ()
runDemo = do
  putStrLn "Loading game data..."
  (gen1Data, gen2Data) <- loadOrDie =<< loadAllGameData
  putStrLn $ "  Gen 1: " ++ show (Map.size (gameSpecies (gameSpeciesGraph gen1Data))) ++ " species, "
           ++ show (Map.size (gameMoves (gameLookupTables gen1Data))) ++ " moves, "
           ++ show (Map.size (gameMachines (gameMachineData gen1Data))) ++ " TM/HMs"
  putStrLn $ "  Gen 2: " ++ show (Map.size (gameSpecies (gameSpeciesGraph gen2Data))) ++ " species, "
           ++ show (Map.size (gameMoves (gameLookupTables gen2Data))) ++ " moves, "
           ++ show (Map.size (gameMachines (gameMachineData gen2Data))) ++ " TM/HMs"
  putStrLn $ "         " ++ show (Map.size (gameEggMoves (gameLearnsetData gen2Data))) ++ " species w/ egg moves, "
           ++ show (Map.size (gameTutorMoves (gameLearnsetData gen2Data))) ++ " species w/ tutor moves"

  section "Stat Calculation"
  demoStats gen1Data gen2Data

  section "Move Legality"
  demoLegality gen1Data gen2Data

  section "Text Codec"
  demoTextCodec


-- ── Demo Helpers ──────────────────────────────────────────────

section :: String -> IO ()
section title = putStrLn $
  "\x2500\x2500 " ++ title ++ " " ++ replicate (50 - 4 - length title) '\x2500'

-- | Find a species by name in a GameData.
findSpecies :: GameData -> Text -> Maybe (DexNumber, Species)
findSpecies gameData name = do
  dex <- Map.lookup name (gameSpeciesByName (gameSpeciesGraph gameData))
  species <- Map.lookup dex (gameSpecies (gameSpeciesGraph gameData))
  pure (dex, species)

-- | Find a move by name in a GameData.
findMove :: GameData -> Text -> Maybe (MoveId, Move)
findMove gameData name = do
  matchedMoveId <- Map.lookup name (gameMoveByName (gameLookupTables gameData))
  move <- Map.lookup matchedMoveId (gameMoves (gameLookupTables gameData))
  pure (matchedMoveId, move)


-- ── Stat Calculation Demo ─────────────────────────────────────

demoStats :: GameData -> GameData -> IO ()
demoStats gen1Data gen2Data = do
  case Map.lookup (DexNumber 25) (gameSpecies (gameSpeciesGraph gen1Data)) of
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

  case Map.lookup (DexNumber 25) (gameSpecies (gameSpeciesGraph gen2Data)) of
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
showSpecial (Unified specialValue)     = "Spc " ++ show specialValue
showSpecial (Split spAttack spDefense) = "SpA " ++ show spAttack ++ "  SpD " ++ show spDefense

showDVs :: DVs -> String
showDVs dvs = "{Atk=" ++ show (dvAttack dvs) ++ " Def=" ++ show (dvDefense dvs)
           ++ " Spd=" ++ show (dvSpeed dvs) ++ " Spc=" ++ show (dvSpecial dvs) ++ "}"


-- ── Move Legality Demo ────────────────────────────────────────

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


classify :: GameData -> Maybe GameData -> Text -> Text -> Level -> IO ()
classify gameData otherGameData targetSpecies targetMove level =
  case (findSpecies gameData targetSpecies, findMove gameData targetMove) of
    (Nothing, _) -> TextIO.putStrLn $ "  " <> targetSpecies <> ": species not found"
    (_, Nothing) -> TextIO.putStrLn $ "  " <> targetMove <> ": move not found"
    (Just (dex, _), Just (targetMoveId, _)) -> do
      let genLabel = case gameGen gameData of Gen1 -> "Gen 1"; Gen2 -> "Gen 2"
          sources = classifyMove gameData otherGameData dex targetMoveId level
      TextIO.putStrLn $ "  " <> targetSpecies <> " + " <> targetMove
                   <> " (" <> genLabel <> ", L" <> Text.pack (show (unLevel level)) <> "):"
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
      methodLabel (sourceMethod source) ++ " (" ++ Text.unpack (sourceDetail source) ++ ")"


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


-- ── Text Codec Demo ───────────────────────────────────────────

demoTextCodec :: IO ()
demoTextCodec = do
  (codec, screens) <- loadOrDie =<< loadCodec Gen1 English
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
  TextIO.putStrLn $ "  Decode back      \x2192 " <> displayText decoded

  -- Ligature round-trip (the PK and MN symbols from the games)
  let encodeLigature text = case lookupLigature codec text of
        Just gameChar -> gameChar
        Nothing       -> error $ "Ligature not in codec: " ++ Text.unpack text
      ligatureText    = GameText [encodeLigature "PK", encodeLigature "MN"]
      ligatureEncoded = encodeText 11 ligatureText
      ligatureDecoded = decodeText codec ligatureEncoded
  putStrLn $ "  Encode [PK][MN]  \x2192 " ++ showHexBytes ligatureEncoded
  TextIO.putStrLn $ "  Decode back      \x2192 " <> displayText ligatureDecoded

  -- Naming screen info
  putStrLn $ "  Naming screens: " ++ show (length screens)
  mapM_ showScreen screens


showScreen :: NamingScreen -> IO ()
showScreen screen = TextIO.putStrLn $
  "    " <> screenLabel screen <> ": "
  <> Text.pack (show (Set.size (screenChars screen))) <> " choosable characters"


-- | Show a ByteString as space-separated hex pairs.
showHexBytes :: ByteString -> String
showHexBytes bytes = unwords [ showHexByte byte | byte <- ByteString.unpack bytes ]
