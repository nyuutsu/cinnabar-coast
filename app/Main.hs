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
  ( interpretGen1Save, InterpretedSave (..), InterpretedPokemon (..)
  , InterpretedSpecies (..), InterpretedMove (..)
  , EeveelutionPath (..), EeveelutionState (..), RivalStarter (..)
  , InterpretedDaycare (..)
  , WarningContext (..), SaveWarning (..)
  , InterpretedBox (..), InterpretedHoFEntry (..), InterpretedHoFRecord (..)
  , InterpretedProgress (..), MovementMode (..), FlagState (..), MapScriptState (..)
  , InventoryEntry (..), PlayTime (..)
  )
import Cinnabar.Save.Layout
  ( cartridgeLayout, CartridgeLayout (..), BoxCapacity (..)
  , GameVariant (..), SaveRegion (..)
  )
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
    ["flags", savePath] -> runFlagsCommand savePath
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
  hPutStrLn stderr "  flags <file>    Show all event flags and progress"
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


-- ── Flags Command ─────────────────────────────────────────────

runFlagsCommand :: FilePath -> IO ()
runFlagsCommand savePath = do
  interpreted <- loadGen1Save savePath
  printAllFlags (interpProgress interpreted)


-- ── Save Display ──────────────────────────────────────────────

printSaveSummary :: InterpretedSave -> IO ()
printSaveSummary interpreted = do
  TextIO.putStrLn $ "Player: " <> displayText (interpPlayerName interpreted)
    <> " (ID: " <> Text.pack (show (unTrainerId (interpPlayerID interpreted))) <> ")"
  TextIO.putStrLn $ "Rival: " <> displayText (interpRivalName interpreted)
  let time = interpPlayTime interpreted
      timeStr = show (playHours time) ++ ":" ++ zeroPadTwo (playMinutes time)
                  ++ ":" ++ zeroPadTwo (playSeconds time)
      maxedStr = if interpPlayTimeMaxed interpreted then " (maxed)" else ""
  putStrLn $ "Time: " ++ timeStr ++ maxedStr
  putStrLn $ "Money: " ++ formatMoney (interpMoney interpreted)
  putStrLn $ "Coins: " ++ show (interpCasinoCoins interpreted)
  putStrLn ""

  let earnedBadges = filter flagIsSet (progBadges (interpProgress interpreted))
  case earnedBadges of
    [] -> putStrLn "Badges: (none)"
    _  -> TextIO.putStrLn $ "Badges: "
      <> Text.intercalate ", " (map flagName earnedBadges)
  let ownedCount = Set.size (interpPokedexOwned interpreted)
      seenCount  = Set.size (interpPokedexSeen interpreted)
  putStrLn $ "Pokédex: " ++ show ownedCount ++ " owned, " ++ show seenCount ++ " seen"
  putStrLn ""

  printProgressSummary (interpProgress interpreted)

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

  case interpPikachuHappiness interpreted of
    Just happiness -> do
      let moodStr = case interpPikachuMood interpreted of
            Just mood -> ", mood " ++ show mood
            Nothing   -> ""
      putStrLn $ "Pikachu: happiness " ++ show happiness ++ moodStr
    Nothing -> pure ()
  case interpSurfingHiScore interpreted of
    Just score -> putStrLn $ "Surfing Pikachu hi-score: " ++ show score
    Nothing -> pure ()
  case interpPrinterSettings interpreted of
    Just settings -> putStrLn $ "Printer settings: 0x" ++ showHexByte settings
    Nothing -> pure ()
  putStrLn $ "Position: (" ++ show (interpPlayerY interpreted)
    ++ ", " ++ show (interpPlayerX interpreted)
    ++ ") on map " ++ show (interpCurrentMap interpreted)
    ++ ", previous map " ++ show (interpPreviousMap interpreted)
    ++ ", blackout map " ++ show (interpLastBlackoutMap interpreted)
  if interpInSafari interpreted
    then putStrLn $ "Safari Zone: " ++ show (interpSafariSteps interpreted)
      ++ " steps remaining, " ++ show (interpSafariBallCount interpreted) ++ " balls"
    else pure ()
  case interpFossilItem interpreted of
    Just itemName -> TextIO.putStrLn $ "Fossil: given " <> itemName
      <> case interpFossilResult interpreted of
           Just species -> ", result " <> Text.pack (renderSpecies species)
           Nothing      -> ""
    Nothing -> pure ()
  case interpDaycare interpreted of
    Just daycare -> TextIO.putStrLn $ "Daycare: "
      <> Text.pack (renderSpecies (daycareSpecies daycare))
      <> " \"" <> displayText (daycareNickname daycare)
      <> "\" (OT: " <> displayText (daycareOTName daycare) <> ")"
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
  mapM_ (uncurry printPartyPokemon) (zip [1 ..] party)
  case interpWarnings interpreted of
    [] -> pure ()
    warnings -> do
      putStrLn $ "Warnings (" ++ show (length warnings) ++ "):"
      mapM_ (putStrLn . ("  " ++) . renderWarning) warnings


printPartyPokemon :: Int -> InterpretedPokemon -> IO ()
printPartyPokemon slotNumber pokemon = do
  let speciesLabel  = renderSpecies (interpSpecies pokemon)
      levelValue    = unLevel (interpLevel pokemon)
      nicknameLabel = Text.unpack (displayText (interpNickname pokemon))
      moveLabels    = mapMaybe renderMove (interpMoves pokemon)
      dvs           = interpDVs pokemon
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
  putStrLn $ "     HP: " ++ show (interpCurrentHP pokemon)
    ++ "/" ++ show (interpMaxHP pokemon)
  putStrLn ""


printBoxSummary :: InterpretedSave -> IO ()
printBoxSummary interpreted = do
  let activeBoxNum = interpActiveBoxNum interpreted
      boxCapacity = case interpRaw interpreted of
        RawGen1Save raw -> unBoxCapacity (layoutBoxCapacity (rawGen1Layout raw))
        _ -> 20
      boxes = interpPCBoxes interpreted
      nonEmptyNums = Set.fromList (map interpBoxNumber boxes)
      displayNums = Set.toAscList (Set.insert activeBoxNum nonEmptyNums)
      boxCountMap = Map.fromList
        [(interpBoxNumber box, length (interpBoxMembers box)) | box <- boxes]
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
        RawGen1Save raw -> unBoxCapacity (layoutBoxCapacity (rawGen1Layout raw))
        _ -> 20
  if null boxes
    then putStrLn "No Pokémon in PC boxes."
    else mapM_ (printBoxDetail boxCapacity) boxes


printBoxDetail :: Int -> InterpretedBox -> IO ()
printBoxDetail boxCapacity box = do
  let count = length (interpBoxMembers box)
  putStrLn $ "Box " ++ show (interpBoxNumber box)
    ++ " (" ++ show count ++ "/" ++ show boxCapacity ++ "):"
  mapM_ (uncurry printPartyPokemon) (zip [1 ..] (interpBoxMembers box))


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
formatWithCommas amount
  | amount < 1000  = show amount
  | otherwise = formatWithCommas (amount `div` 1000) ++ "," ++ zeroPadThree (amount `mod` 1000)
  where
    zeroPadThree value = let digits = show value in replicate (3 - length digits) '0' ++ digits

zeroPadTwo :: Int -> String
zeroPadTwo value
  | value < 10    = "0" ++ show value
  | otherwise = show value


renderSpecies :: InterpretedSpecies -> String
renderSpecies (KnownSpecies _ species) = Text.unpack (speciesName species)
renderSpecies (UnknownSpecies idx) =
  "Unknown [0x" ++ showHexByte (unInternalIndex idx) ++ "]"
renderSpecies (UnknownDexSpecies dex) =
  "Unknown [#" ++ show (unDex dex) ++ "]"


renderRivalStarter :: RivalStarter -> String
renderRivalStarter (RivalStarterSpecies species) = renderSpecies species
renderRivalStarter (RivalEeveelution state) = renderEeveelutionState state

renderRivalStarterLabel :: RivalStarter -> String
renderRivalStarterLabel (RivalStarterSpecies _) = "Rival's starter"
renderRivalStarterLabel (RivalEeveelution _) = "Rival's Eevee"

renderEeveelutionState :: EeveelutionState -> String
renderEeveelutionState EeveelutionPending = "not yet determined"
renderEeveelutionState (EeveelutionKnown JolteonPath) = "Jolteon path"
renderEeveelutionState (EeveelutionKnown FlareonPath) = "Flareon path"
renderEeveelutionState (EeveelutionKnown VaporeonPath) = "Vaporeon path"
renderEeveelutionState (EeveelutionUnknown byte) =
  "unknown (0x" ++ showHexByte byte ++ ")"


renderMove :: InterpretedMove -> Maybe String
renderMove EmptyMove          = Nothing
renderMove (KnownMove _ move) = Just (Text.unpack (moveName move))
renderMove (UnknownMove byte) = Just ("Unknown [0x" ++ showHexByte byte ++ "]")

renderMovementMode :: MovementMode -> String
renderMovementMode Walking = "Walking"
renderMovementMode Biking = "Biking"
renderMovementMode Surfing = "Surfing"
renderMovementMode (UnknownMovement byte) = "Unknown (0x" ++ showHexByte byte ++ ")"


renderSaveError :: SaveError -> String
renderSaveError (WrongFileSize expected actual) =
  "wrong file size: expected " ++ show expected ++ " bytes, got " ++ show actual
renderSaveError (UnsupportedLayout description) =
  "unsupported layout: " ++ Text.unpack description
renderSaveError (UnimplementedGen gen) =
  "unimplemented: " ++ show gen


renderWarningContext :: WarningContext -> String
renderWarningContext (PartySlot slot)        = "Party slot " ++ show slot
renderWarningContext (BoxSlot box slot)      = "Box " ++ show box ++ " slot " ++ show slot
renderWarningContext (HoFSlot record entry)  = "HoF record " ++ show record ++ " slot " ++ show entry
renderWarningContext PlayerStarter           = "Player starter"
renderWarningContext RivalStarterSlot        = "Rival starter"
renderWarningContext DaycareSlot             = "Daycare"
renderWarningContext FossilSlot              = "Fossil"

renderWarning :: SaveWarning -> String
renderWarning (UnknownSpeciesIndex context idx) =
  renderWarningContext context ++ ": unknown species index 0x"
    ++ showHexByte (unInternalIndex idx)
renderWarning (UnknownMoveId context moveSlot byte) =
  renderWarningContext context ++ ", move " ++ show moveSlot
    ++ ": unknown move ID 0x" ++ showHexByte byte
renderWarning (SpeciesListMismatch context listByte structByte) =
  renderWarningContext context ++ ": species list/struct mismatch (0x"
    ++ showHexByte listByte ++ " vs 0x" ++ showHexByte structByte ++ ")"
renderWarning (ChecksumMismatch stored calculated) =
  "Checksum mismatch: stored 0x" ++ showHexByte stored
    ++ ", calculated 0x" ++ showHexByte calculated
renderWarning (StatMismatch context statName stored calculated) =
  renderWarningContext context ++ ": " ++ Text.unpack statName
    ++ " mismatch (stored " ++ show stored
    ++ ", calculated " ++ show calculated ++ ")"
renderWarning (BoxBankChecksumMismatch bankIndex stored calculated) =
  "Box bank " ++ show bankIndex ++ ": checksum mismatch (stored 0x"
    ++ showHexByte stored ++ ", calculated 0x" ++ showHexByte calculated ++ ")"
renderWarning (BoxChecksumMismatch bankIndex boxIndex stored calculated) =
  "Box bank " ++ show bankIndex ++ ", box " ++ show boxIndex
    ++ ": checksum mismatch (stored 0x" ++ showHexByte stored
    ++ ", calculated 0x" ++ showHexByte calculated ++ ")"
renderWarning ActiveBoxDesync =
  "Active box data differs from its PC bank copy (desync)"
renderWarning (UnexpectedEeveelution byte) =
  renderWarningContext RivalStarterSlot ++ ": unexpected Eeveelution byte 0x"
    ++ showHexByte byte


-- ── Progress Display ──────────────────────────────────────────

printProgressSummary :: InterpretedProgress -> IO ()
printProgressSummary progress = do
  let rivalLabel = renderRivalStarterLabel (progRivalStarter progress)
      rivalValue = renderRivalStarter (progRivalStarter progress)
  putStrLn $ "Starter: " ++ renderSpecies (progPlayerStarter progress)
    ++ "    " ++ rivalLabel ++ ": " ++ rivalValue

  let defeatedGyms = filter flagIsSet (progDefeatedGyms progress)
  case defeatedGyms of
    [] -> pure ()
    _  -> TextIO.putStrLn $ "Gyms defeated: "
      <> Text.intercalate ", " (map flagName defeatedGyms)

  let rods = concat
        [ ["Old Rod" | progReceivedOldRod progress]
        , ["Good Rod" | progReceivedGoodRod progress]
        , ["Super Rod" | progReceivedSuperRod progress]
        ]
  case rods of
    [] -> pure ()
    _  -> TextIO.putStrLn $ "Rods: " <> Text.intercalate ", " rods

  if progReceivedLapras progress
    then putStrLn "Lapras: received"
    else pure ()

  putStrLn $ "Movement: " ++ renderMovementMode (progMovementMode progress)

  if progTradesCompleted progress > 0
    then putStrLn $ "Trades completed: " ++ show (progTradesCompleted progress)
    else pure ()
  putStrLn ""


printAllFlags :: InterpretedProgress -> IO ()
printAllFlags progress = do
  printFlagSection "Badges" "earned" "not earned" (progBadges progress)
  printFlagSection "Gyms defeated" "defeated" "not defeated" (progDefeatedGyms progress)
  printFlagSection "Towns visited" "visited" "not visited" (progTownsVisited progress)
  printFlagSection "Event flags" "set" "unset" (progEventFlags progress)

  putStrLn "Various flags:"
  putStrLn $ "  Received Old Rod: " ++ showYesNo (progReceivedOldRod progress)
  putStrLn $ "  Received Good Rod: " ++ showYesNo (progReceivedGoodRod progress)
  putStrLn $ "  Received Super Rod: " ++ showYesNo (progReceivedSuperRod progress)
  putStrLn $ "  Received Lapras: " ++ showYesNo (progReceivedLapras progress)
  putStrLn $ "  Received starter: " ++ showYesNo (progReceivedStarter progress)
  putStrLn $ "  Healed at center: " ++ showYesNo (progHealedAtCenter progress)
  putStrLn $ "  Test battle: " ++ showYesNo (progTestBattle progress)
  putStrLn $ "  Prevent music change: " ++ showYesNo (progPreventMusicChange progress)
  putStrLn $ "  Trainer wants battle: " ++ showYesNo (progTrainerWantsBattle progress)
  putStrLn $ "  Used Fly: " ++ showYesNo (progUsedFly progress)
  putStrLn $ "  Standing on door: " ++ showYesNo (progStandingOnDoor progress)
  putStrLn $ "  Stepping from door: " ++ showYesNo (progSteppingFromDoor progress)
  putStrLn $ "  Standing on warp: " ++ showYesNo (progStandingOnWarp progress)
  putStrLn $ "  Jumping ledge: " ++ showYesNo (progJumpingLedge progress)
  putStrLn $ "  Spinning: " ++ showYesNo (progSpinning progress)
  putStrLn $ "  Beaten Lorelei (E4 run): " ++ showYesNo (progBeatenLorelei progress)
  putStrLn ""

  printFlagSection "Toggleable objects" "hidden" "visible" (progToggleFlags progress)

  let scripts      = progMapScripts progress
      nonZeroCount = length (filter (\state -> scriptStep state /= 0) scripts)
  putStrLn $ "Map script progress (" ++ show nonZeroCount ++ " non-zero of "
    ++ show (length scripts) ++ " named):"
  if null scripts
    then putStrLn "  (none)"
    else mapM_ (\state -> TextIO.putStrLn $ "  " <> padRight 40 (scriptName state)
      <> "step " <> Text.pack (show (scriptStep state))) scripts

printFlagSection :: String -> Text -> Text -> [FlagState] -> IO ()
printFlagSection title setLabel unsetLabel flags = do
  let setCount   = length (filter flagIsSet flags)
      totalCount = length flags
  putStrLn $ title ++ " (" ++ show setCount ++ "/" ++ show totalCount ++ "):"
  if null flags
    then putStrLn "  (none)"
    else mapM_ (\flag -> TextIO.putStrLn $ "  " <> padRight 40 (flagName flag)
      <> if flagIsSet flag then setLabel else unsetLabel) flags
  putStrLn ""

padRight :: Int -> Text -> Text
padRight width text
  | Text.length text >= width = text <> " "
  | otherwise = text <> Text.replicate (width - Text.length text) " "

showYesNo :: Bool -> String
showYesNo True  = "yes"
showYesNo False = "no"

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
