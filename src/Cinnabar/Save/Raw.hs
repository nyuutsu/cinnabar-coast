{-# LANGUAGE OverloadedStrings #-}

-- | Top-level save file types and parser.
--
-- Takes a CartridgeLayout and raw bytes, validates the file, and
-- dispatches to the generation-specific parser. Currently supports
-- Gen 1; Gen 2 returns UnimplementedGen.

module Cinnabar.Save.Raw
  ( -- * Errors
    SaveError (..)

    -- * Raw save types
  , RawSaveFile (..)
  , RawGen1SaveFile (..)
  , RawGen2SaveFile (..)

    -- * Sub-record types
  , RawItemEntry (..)
  , RawPlayTime (..)
  , RawDaycare (..)
  , RawProgressFlags (..)
  , RawPlayerPosition (..)
  , RawSafariState (..)
  , RawFossilState (..)
  , RawTransientState (..)

    -- * Hall of Fame types
  , RawGen1HoFEntry (..)
  , RawGen1HoFRecord (..)

    -- * Container types
  , RawGen1Party (..)
  , RawGen1Box (..)
  , RawBankValidity (..)

    -- * Parser
  , parseRawSave
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import Data.Word (Word8, Word16)

import Cinnabar.Binary (Cursor, mkCursor, readByte, readWord16BE, readBytes, seekTo, skip)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Gen1.Raw
  ( RawGen1PartyPokemon, RawGen1BoxPokemon
  , parseGen1PartyPokemon, parseGen1BoxPokemon
  )
import Cinnabar.Save.Layout
  ( CartridgeLayout (..)
  , NameLength (..), BoxCapacity (..)
  , SaveOffsets (..)
  , Gen1SaveOffsets (..)
  , BoxBankInfo (..)
  , gen1PartyCapacity, gen1PartyPokemonSize, gen1BoxPokemonSize
  , gen1HoFRecordCount, gen1HoFSlotsPerRecord, gen1HoFEntrySize, gen1HoFRecordSize
  )
import Cinnabar.Types (Gen (..), InternalIndex (..))


-- ── Errors ─────────────────────────────────────────────────────

data SaveError
  = WrongFileSize !Int !Int
  | UnsupportedLayout !Text
  | UnimplementedGen !Gen
  deriving (Eq, Show)


-- ── Raw Save Types ─────────────────────────────────────────────

data RawSaveFile
  = RawGen1Save !RawGen1SaveFile
  | RawGen2Save !RawGen2SaveFile

data RawGen1SaveFile = RawGen1SaveFile
  { rawGen1Bytes            :: !ByteString
  , rawGen1Layout           :: !CartridgeLayout
  , rawGen1PlayerName       :: !ByteString
  , rawGen1RivalName        :: !ByteString
  , rawGen1Party            :: !RawGen1Party
  , rawGen1CurrentBox       :: !RawGen1Box
  , rawGen1Checksum         :: !Word8
  , rawGen1ChecksumValid    :: !Bool
  , rawGen1PokedexOwned     :: !ByteString
  , rawGen1PokedexSeen      :: !ByteString
  , rawGen1BagItems         :: ![RawItemEntry]
  , rawGen1BoxItems         :: ![RawItemEntry]
  , rawGen1Money            :: !ByteString
  , rawGen1CasinoCoins      :: !ByteString
  , rawGen1Badges           :: !Word8
  , rawGen1PlayerID         :: !Word16
  , rawGen1Options          :: !Word8
  , rawGen1CurrentBoxNum    :: !Word8
  , rawGen1HoFCount         :: !Word8
  , rawGen1PlayTime         :: !RawPlayTime
  , rawGen1PikachuHappiness :: !Word8     -- Yellow only; unused in R/B
  , rawGen1PikachuMood     :: !Word8     -- Yellow only; unused in R/B
  , rawGen1SurfingHiScore  :: !ByteString -- 2 bytes, little-endian BCD (Yellow only)
  , rawGen1PrinterSettings :: !Word8     -- Yellow only
  , rawGen1Daycare          :: !RawDaycare
  , rawGen1PCBoxes          :: ![RawGen1Box]
  , rawGen1BoxBankValid     :: ![RawBankValidity]
  , rawGen1HallOfFame       :: ![RawGen1HoFRecord]
  , rawGen1Progress         :: !RawProgressFlags
  , rawGen1PlayerPosition   :: !RawPlayerPosition
  , rawGen1Safari           :: !RawSafariState
  , rawGen1Fossil           :: !RawFossilState
  , rawGen1Transient        :: !RawTransientState
  }

-- | Stub -- will be fleshed out when Gen 2 parsing is implemented.
data RawGen2SaveFile = RawGen2SaveFile


-- ── Sub-Record Types ─────────────────────────────────────────

data RawItemEntry = RawItemEntry
  { rawItemId       :: !Word8
  , rawItemQuantity :: !Word8
  } deriving (Eq, Show)

data RawPlayTime = RawPlayTime
  { rawPlayHours   :: !Word8
  , rawPlayMaxed   :: !Word8   -- nonzero if the timer has capped
  , rawPlayMinutes :: !Word8
  , rawPlaySeconds :: !Word8
  , rawPlayFrames  :: !Word8   -- 1/60th second
  } deriving (Eq, Show)

data RawDaycare = RawDaycare
  { rawDaycareInUse    :: !Word8            -- 0 = empty, nonzero = occupied
  , rawDaycarePokemon      :: !InternalIndex    -- species if in use
  , rawDaycareNickname :: !ByteString       -- 11 bytes, raw text
  , rawDaycareOTName   :: !ByteString       -- 11 bytes, raw text
  } deriving (Eq, Show)

data RawProgressFlags = RawProgressFlags
  { rawEventFlags     :: !ByteString     -- 320 bytes, bit-packed
  , rawToggleFlags    :: !ByteString     -- 32 bytes, bit-packed (toggleable objects)
  , rawMapScripts     :: !ByteString     -- 256 bytes (per-map script progress counters)
  , rawDefeatedGyms   :: !Word8          -- bitfield
  , rawPlayerStarter  :: !InternalIndex
  , rawRivalStarter   :: !InternalIndex
  , rawTownsVisited   :: !ByteString     -- 2 bytes, bitfield
  , rawMovementStatus :: !Word8
  , rawVarFlags1      :: !Word8
  , rawVarFlags2      :: !Word8
  , rawVarFlags3      :: !Word8
  , rawVarFlags4      :: !Word8
  , rawVarFlags5      :: !Word8
  , rawVarFlags6      :: !Word8
  , rawVarFlags7      :: !Word8
  , rawVarFlags8      :: !Word8
  , rawDefeatedLorelei :: !ByteString    -- 2 bytes, bit 1 of byte 0 tracks E4 run
  , rawInGameTrades   :: !ByteString     -- 2 bytes, bitset
  , rawHiddenItems    :: !ByteString     -- 14 bytes
  , rawHiddenCoins    :: !ByteString     -- 2 bytes
  , rawCurrentMap     :: !Word8
  } deriving (Eq, Show)

data RawPlayerPosition = RawPlayerPosition
  { rawPlayerY          :: !Word8
  , rawPlayerX          :: !Word8
  , rawLastMap          :: !Word8
  , rawLastBlackoutMap  :: !Word8
  , rawDestinationMap   :: !Word8
  } deriving (Eq, Show)

data RawSafariState = RawSafariState
  { rawSafariSteps     :: !Word16       -- big-endian
  , rawSafariBallCount :: !Word8
  , rawSafariGameOver  :: !Word8
  } deriving (Eq, Show)

data RawFossilState = RawFossilState
  { rawFossilItemGiven :: !Word8        -- item byte
  , rawFossilResult    :: !ByteString   -- 3 bytes
  } deriving (Eq, Show)

data RawTransientState = RawTransientState
  { rawLetterDelay        :: !Word8
  , rawMusicId            :: !Word8
  , rawMusicBank          :: !Word8
  , rawContrastId         :: !Word8
  , rawEnemyTrainerClass  :: !Word8
  , rawBoulderSpriteIndex :: !Word8
  , rawDungeonWarpDest    :: !Word8
  , rawDungeonWarpUsed    :: !Word8
  , rawWarpedFromWarp     :: !Word8
  , rawWarpedFromMap      :: !Word8
  , rawCardKeyDoorY       :: !Word8
  , rawCardKeyDoorX       :: !Word8
  , rawTrashCanLock1      :: !Word8
  , rawTrashCanLock2      :: !Word8
  , rawCurrentMapScript   :: !Word8
  } deriving (Eq, Show)

data RawGen1HoFEntry = RawGen1HoFEntry
  { rawGen1HoFSpecies  :: !InternalIndex   -- 0x00 = empty slot
  , rawGen1HoFLevel    :: !Word8
  , rawGen1HoFNickname :: !ByteString      -- 11 bytes, raw text
  , rawGen1HoFPadding  :: !ByteString      -- 3 bytes, preserved for round-trip
  } deriving (Eq, Show)

data RawGen1HoFRecord = RawGen1HoFRecord
  { rawGen1HoFEntries :: ![RawGen1HoFEntry]    -- all 6 slots
  } deriving (Eq, Show)


-- ── Container Types ────────────────────────────────────────────

data RawGen1Party = RawGen1Party
  { rawGen1PartyCount   :: !Word8
  , rawGen1PartySpecies :: ![InternalIndex]
  , rawGen1PartyMembers    :: ![RawGen1PartyPokemon]
  , rawGen1PartyOTNames :: ![ByteString]
  , rawGen1PartyNicknames   :: ![ByteString]
  } deriving (Eq, Show)

data RawBankValidity = RawBankValidity
  { bankChecksumValid :: !Bool
  , boxChecksumsValid :: ![Bool]
  } deriving (Eq, Show)

data RawGen1Box = RawGen1Box
  { rawGen1BoxCount   :: !Word8
  , rawGen1BoxSpecies :: ![InternalIndex]
  , rawGen1BoxMembers    :: ![RawGen1BoxPokemon]
  , rawGen1BoxOTNames :: ![ByteString]
  , rawGen1BoxNicknames   :: ![ByteString]
  } deriving (Eq, Show)


-- ── Top-Level Parser ───────────────────────────────────────────

gen1FileSize :: Int
gen1FileSize = 32768

parseRawSave :: CartridgeLayout -> ByteString -> Either SaveError RawSaveFile
parseRawSave layout bytes = case layoutGen layout of
  Gen1
    | ByteString.length bytes /= gen1FileSize ->
        Left (WrongFileSize gen1FileSize (ByteString.length bytes))
    | Gen1Offsets offsets <- layoutOffsets layout ->
        Right (RawGen1Save (parseGen1Save layout offsets bytes))
    | otherwise ->
        Left (UnsupportedLayout "Gen 1 game with Gen 2 offsets")
  Gen2 -> Left (UnimplementedGen Gen2)


-- ── Gen 1 Save Parsing ────────────────────────────────────────

parseGen1Save :: CartridgeLayout -> Gen1SaveOffsets -> ByteString -> RawGen1SaveFile
parseGen1Save layout offsets bytes =
  let cursor      = mkCursor bytes
      nameLen     = layoutNameLen layout
      boxCapacity = layoutBoxCapacity layout

      (playerName, _) = readBytes (unNameLength nameLen) (seekTo (g1PlayerName offsets) cursor)
      (rivalName, _)  = readBytes (unNameLength nameLen) (seekTo (g1RivalName offsets) cursor)
      (party, _)      = parseGen1Party nameLen (seekTo (g1PartyData offsets) cursor)
      (currentBox, _) = parseGen1Box nameLen boxCapacity (seekTo (g1CurrentBox offsets) cursor)

      (pokedexOwned, _) = readBytes 19 (seekTo (g1PokedexOwned offsets) cursor)
      (pokedexSeen, _)  = readBytes 19 (seekTo (g1PokedexSeen offsets) cursor)
      (bagItems, _)     = parseItemList (seekTo (g1BagItems offsets) cursor)
      (boxItems, _)     = parseItemList (seekTo (g1BoxItems offsets) cursor)
      (money, _)        = readBytes 3 (seekTo (g1Money offsets) cursor)
      (casinoCoins, _)  = readBytes 2 (seekTo (g1CasinoCoins offsets) cursor)
      (badges, _)       = readByte (seekTo (g1Badges offsets) cursor)
      (playerID, _)     = readWord16BE (seekTo (g1PlayerID offsets) cursor)
      (options, _)      = readByte (seekTo (g1Options offsets) cursor)
      (boxNumber, _)    = readByte (seekTo (g1CurrentBoxNumber offsets) cursor)
      (hofCount, _)     = readByte (seekTo (g1HoFCount offsets) cursor)
      playTime          = parseRawPlayTime (seekTo (g1PlayTime offsets) cursor)
      (pikachuHappiness, _) = readByte (seekTo (g1PikachuHappiness offsets) cursor)
      (pikachuMood, _)      = readByte (seekTo (g1PikachuMood offsets) cursor)
      (surfingHiScore, _)   = readBytes 2 (seekTo (g1SurfingHiScore offsets) cursor)
      (printerSettings, _)  = readByte (seekTo (g1PrinterSettings offsets) cursor)
      daycare           = parseRawDaycare nameLen offsets cursor
      progress          = parseRawProgress offsets cursor
      playerPosition    = parseRawPlayerPosition offsets cursor
      safari            = parseRawSafari offsets cursor
      fossil            = parseRawFossil offsets cursor
      transient         = parseRawTransient offsets cursor

      hallOfFame = parseHallOfFame nameLen (seekTo (g1HallOfFame offsets) cursor)

      (pcBoxes, boxBankValidity) = parseBoxBanks nameLen boxCapacity bytes (g1BoxBanks offsets)

      storedChecksum     = ByteString.index bytes (g1Checksum offsets)
      calculatedChecksum = calculateGen1Checksum bytes
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)

  in RawGen1SaveFile
      { rawGen1Bytes            = bytes
      , rawGen1Layout           = layout
      , rawGen1PlayerName       = playerName
      , rawGen1RivalName        = rivalName
      , rawGen1Party            = party
      , rawGen1CurrentBox       = currentBox
      , rawGen1Checksum         = storedChecksum
      , rawGen1ChecksumValid    = storedChecksum == calculatedChecksum
      , rawGen1PokedexOwned     = pokedexOwned
      , rawGen1PokedexSeen      = pokedexSeen
      , rawGen1BagItems         = bagItems
      , rawGen1BoxItems         = boxItems
      , rawGen1Money            = money
      , rawGen1CasinoCoins      = casinoCoins
      , rawGen1Badges           = badges
      , rawGen1PlayerID         = playerID
      , rawGen1Options          = options
      , rawGen1CurrentBoxNum    = boxNumber
      , rawGen1HoFCount         = hofCount
      , rawGen1PlayTime         = playTime
      , rawGen1PikachuHappiness = pikachuHappiness
      , rawGen1PikachuMood     = pikachuMood
      , rawGen1SurfingHiScore  = surfingHiScore
      , rawGen1PrinterSettings = printerSettings
      , rawGen1Daycare          = daycare
      , rawGen1PCBoxes          = pcBoxes
      , rawGen1BoxBankValid     = boxBankValidity
      , rawGen1HallOfFame       = hallOfFame
      , rawGen1Progress         = progress
      , rawGen1PlayerPosition   = playerPosition
      , rawGen1Safari           = safari
      , rawGen1Fossil           = fossil
      , rawGen1Transient        = transient
      }


-- ── Item List Parser ─────────────────────────────────────────

-- | Parse a Gen 1 item list: 1 byte count, count x (item ID, quantity)
-- pairs, then a 0xFF terminator.
parseItemList :: Cursor -> ([RawItemEntry], Cursor)
parseItemList cursor0 =
  let (count, cursor1) = readByte cursor0
      itemCount        = fromIntegral count :: Int
      (items, cursor2) = readItemPairs itemCount cursor1
      (_terminator, cursor3) = readByte cursor2
  in (items, cursor3)
  where
    readItemPairs :: Int -> Cursor -> ([RawItemEntry], Cursor)
    readItemPairs 0 cursor = ([], cursor)
    readItemPairs remaining cursor =
      let (itemByte, cursor1)    = readByte cursor
          (quantityByte, cursor2) = readByte cursor1
          entry = RawItemEntry { rawItemId = itemByte, rawItemQuantity = quantityByte }
          (rest, cursor3)        = readItemPairs (remaining - 1) cursor2
      in (entry : rest, cursor3)


-- ── Play Time / Daycare Parsers ─────────────────────────────

parseRawPlayTime :: Cursor -> RawPlayTime
parseRawPlayTime cursor0 =
  let (hours, cursor1)   = readByte cursor0
      (maxed, cursor2)   = readByte cursor1
      (minutes, cursor3) = readByte cursor2
      (seconds, cursor4) = readByte cursor3
      (frames, _)        = readByte cursor4
  in RawPlayTime
      { rawPlayHours   = hours
      , rawPlayMaxed   = maxed
      , rawPlayMinutes = minutes
      , rawPlaySeconds = seconds
      , rawPlayFrames  = frames
      }

parseRawDaycare :: NameLength -> Gen1SaveOffsets -> Cursor -> RawDaycare
parseRawDaycare nameLen offsets cursor =
  let nameLenInt       = unNameLength nameLen
      (inUse, _)       = readByte (seekTo (g1DaycareInUse offsets) cursor)
      (speciesByte, _) = readByte (seekTo (g1DaycarePokemon offsets) cursor)
      (nickname, _)    = readBytes nameLenInt (seekTo (g1DaycareNickname offsets) cursor)
      (otName, _)      = readBytes nameLenInt (seekTo (g1DaycareOTName offsets) cursor)
  in RawDaycare
      { rawDaycareInUse    = inUse
      , rawDaycarePokemon      = InternalIndex speciesByte
      , rawDaycareNickname = nickname
      , rawDaycareOTName   = otName
      }

parseRawProgress :: Gen1SaveOffsets -> Cursor -> RawProgressFlags
parseRawProgress offsets cursor =
  let (eventFlags, _)      = readBytes 320 (seekTo (g1EventFlags offsets) cursor)
      (toggleFlags, _)     = readBytes 32 (seekTo (g1ToggleFlags offsets) cursor)
      (mapScripts, _)      = readBytes 256 (seekTo (g1MapScripts offsets) cursor)
      (defeatedGyms, _)    = readByte (seekTo (g1DefeatedGyms offsets) cursor)
      (playerStarter, _)   = readByte (seekTo (g1PlayerStarter offsets) cursor)
      (rivalStarter, _)    = readByte (seekTo (g1RivalStarter offsets) cursor)
      (townsVisited, _)    = readBytes 2 (seekTo (g1TownsVisited offsets) cursor)
      (movementStatus, _)  = readByte (seekTo (g1MovementStatus offsets) cursor)
      (varFlags1, _)       = readByte (seekTo (g1VarFlags1 offsets) cursor)
      (varFlags2, _)       = readByte (seekTo (g1VarFlags2 offsets) cursor)
      (varFlags3, _)       = readByte (seekTo (g1VarFlags3 offsets) cursor)
      (varFlags4, _)       = readByte (seekTo (g1VarFlags4 offsets) cursor)
      (varFlags5, _)       = readByte (seekTo (g1VarFlags5 offsets) cursor)
      (varFlags6, _)          = readByte (seekTo (g1VarFlags6 offsets) cursor)
      (varFlags7, _)          = readByte (seekTo (g1VarFlags7 offsets) cursor)
      (varFlags8, _)          = readByte (seekTo (g1VarFlags8 offsets) cursor)
      (defeatedLorelei, _)    = readBytes 2 (seekTo (g1DefeatedLorelei offsets) cursor)
      (inGameTrades, _)       = readBytes 2 (seekTo (g1InGameTrades offsets) cursor)
      (hiddenItems, _)        = readBytes 14 (seekTo (g1HiddenItems offsets) cursor)
      (hiddenCoins, _)        = readBytes 2 (seekTo (g1HiddenCoins offsets) cursor)
      (currentMap, _)         = readByte (seekTo (g1CurrentMap offsets) cursor)
  in RawProgressFlags
      { rawEventFlags      = eventFlags
      , rawToggleFlags     = toggleFlags
      , rawMapScripts      = mapScripts
      , rawDefeatedGyms    = defeatedGyms
      , rawPlayerStarter   = InternalIndex playerStarter
      , rawRivalStarter    = InternalIndex rivalStarter
      , rawTownsVisited    = townsVisited
      , rawMovementStatus  = movementStatus
      , rawVarFlags1       = varFlags1
      , rawVarFlags2       = varFlags2
      , rawVarFlags3       = varFlags3
      , rawVarFlags4       = varFlags4
      , rawVarFlags5       = varFlags5
      , rawVarFlags6       = varFlags6
      , rawVarFlags7       = varFlags7
      , rawVarFlags8       = varFlags8
      , rawDefeatedLorelei = defeatedLorelei
      , rawInGameTrades    = inGameTrades
      , rawHiddenItems     = hiddenItems
      , rawHiddenCoins     = hiddenCoins
      , rawCurrentMap      = currentMap
      }

parseRawPlayerPosition :: Gen1SaveOffsets -> Cursor -> RawPlayerPosition
parseRawPlayerPosition offsets cursor =
  let (playerY, _)        = readByte (seekTo (g1PlayerY offsets) cursor)
      (playerX, _)        = readByte (seekTo (g1PlayerX offsets) cursor)
      (lastMap, _)        = readByte (seekTo (g1LastMap offsets) cursor)
      (blackoutMap, _)    = readByte (seekTo (g1LastBlackoutMap offsets) cursor)
      (destinationMap, _) = readByte (seekTo (g1DestinationMap offsets) cursor)
  in RawPlayerPosition
      { rawPlayerY         = playerY
      , rawPlayerX         = playerX
      , rawLastMap         = lastMap
      , rawLastBlackoutMap = blackoutMap
      , rawDestinationMap  = destinationMap
      }

parseRawSafari :: Gen1SaveOffsets -> Cursor -> RawSafariState
parseRawSafari offsets cursor =
  let (safariSteps, _) = readWord16BE (seekTo (g1SafariSteps offsets) cursor)
      (ballCount, _)   = readByte (seekTo (g1SafariBallCount offsets) cursor)
      (gameOver, _)    = readByte (seekTo (g1SafariGameOver offsets) cursor)
  in RawSafariState
      { rawSafariSteps     = safariSteps
      , rawSafariBallCount = ballCount
      , rawSafariGameOver  = gameOver
      }

parseRawFossil :: Gen1SaveOffsets -> Cursor -> RawFossilState
parseRawFossil offsets cursor =
  let (itemGiven, _)     = readByte (seekTo (g1FossilItem offsets) cursor)
      (fossilResult, _)  = readBytes 3 (seekTo (g1FossilResult offsets) cursor)
  in RawFossilState
      { rawFossilItemGiven = itemGiven
      , rawFossilResult    = fossilResult
      }

parseRawTransient :: Gen1SaveOffsets -> Cursor -> RawTransientState
parseRawTransient offsets cursor =
  let (letterDelay, _)     = readByte (seekTo (g1LetterDelay offsets) cursor)
      (musicId, _)         = readByte (seekTo (g1MusicId offsets) cursor)
      (musicBank, _)       = readByte (seekTo (g1MusicBank offsets) cursor)
      (contrastId, _)      = readByte (seekTo (g1ContrastId offsets) cursor)
      (trainerClass, _)    = readByte (seekTo (g1EnemyTrainerClass offsets) cursor)
      (boulderSprite, _)   = readByte (seekTo (g1BoulderSpriteIndex offsets) cursor)
      (dungeonDest, _)     = readByte (seekTo (g1DungeonWarpDest offsets) cursor)
      (dungeonUsed, _)     = readByte (seekTo (g1DungeonWarpUsed offsets) cursor)
      (warpedWarp, _)      = readByte (seekTo (g1WarpedFromWarp offsets) cursor)
      (warpedMap, _)       = readByte (seekTo (g1WarpedFromMap offsets) cursor)
      (cardKeyY, _)        = readByte (seekTo (g1CardKeyDoorY offsets) cursor)
      (cardKeyX, _)        = readByte (seekTo (g1CardKeyDoorX offsets) cursor)
      (trashLock1, _)      = readByte (seekTo (g1TrashCanLock1 offsets) cursor)
      (trashLock2, _)      = readByte (seekTo (g1TrashCanLock2 offsets) cursor)
      (mapScript, _)       = readByte (seekTo (g1CurrentMapScript offsets) cursor)
  in RawTransientState
      { rawLetterDelay        = letterDelay
      , rawMusicId            = musicId
      , rawMusicBank          = musicBank
      , rawContrastId         = contrastId
      , rawEnemyTrainerClass  = trainerClass
      , rawBoulderSpriteIndex = boulderSprite
      , rawDungeonWarpDest    = dungeonDest
      , rawDungeonWarpUsed    = dungeonUsed
      , rawWarpedFromWarp     = warpedWarp
      , rawWarpedFromMap      = warpedMap
      , rawCardKeyDoorY       = cardKeyY
      , rawCardKeyDoorX       = cardKeyX
      , rawTrashCanLock1      = trashLock1
      , rawTrashCanLock2      = trashLock2
      , rawCurrentMapScript   = mapScript
      }


-- ── Hall of Fame Parser ─────────────────────────────────────────

parseHallOfFame :: NameLength -> Cursor -> [RawGen1HoFRecord]
parseHallOfFame nameLen cursor0 =
  [ parseHoFRecord nameLen (skip (recordIndex * gen1HoFRecordSize) cursor0) | recordIndex <- [0 .. gen1HoFRecordCount - 1] ]

parseHoFRecord :: NameLength -> Cursor -> RawGen1HoFRecord
parseHoFRecord nameLen cursor0 =
  let (entries, _) = parseHoFEntries nameLen gen1HoFSlotsPerRecord cursor0
  in RawGen1HoFRecord { rawGen1HoFEntries = entries }

parseHoFEntries :: NameLength -> Int -> Cursor -> ([RawGen1HoFEntry], Cursor)
parseHoFEntries _ 0 cursor = ([], cursor)
parseHoFEntries nameLen remaining cursor0 =
  let nameLenInt                 = unNameLength nameLen
      paddingLen               = gen1HoFEntrySize - 2 - nameLenInt
      (speciesByte, cursor1)   = readByte cursor0
      (levelByte, cursor2)     = readByte cursor1
      (nickname, cursor3)      = readBytes nameLenInt cursor2
      (padding, cursor4)       = readBytes paddingLen cursor3
      entry = RawGen1HoFEntry
        { rawGen1HoFSpecies  = InternalIndex speciesByte
        , rawGen1HoFLevel    = levelByte
        , rawGen1HoFNickname = nickname
        , rawGen1HoFPadding  = padding
        }
      (rest, finalCursor) = parseHoFEntries nameLen (remaining - 1) cursor4
  in (entry : rest, finalCursor)


-- ── Container Parsers ──────────────────────────────────────────

parseGen1Party :: NameLength -> Cursor -> (RawGen1Party, Cursor)
parseGen1Party nameLen cursor0 =
  let nameLenInt        = unNameLength nameLen
      (count, cursor1)  = readByte cursor0
      entryCount        = fromIntegral count
      speciesListSize   = gen1PartyCapacity + 1
      (species, cursor2) = parseSpeciesList gen1PartyCapacity speciesListSize cursor1
      (members, cursor3)    = parseFixedArray entryCount gen1PartyCapacity
                             gen1PartyPokemonSize parseGen1PartyPokemon cursor2
      (otNames, cursor4) = parseFixedArray entryCount gen1PartyCapacity
                             nameLenInt (readBytes nameLenInt) cursor3
      (nicknames, cursor5)   = parseFixedArray entryCount gen1PartyCapacity
                             nameLenInt (readBytes nameLenInt) cursor4
  in ( RawGen1Party
        { rawGen1PartyCount   = count
        , rawGen1PartySpecies = species
        , rawGen1PartyMembers    = members
        , rawGen1PartyOTNames = otNames
        , rawGen1PartyNicknames   = nicknames
        }
     , cursor5
     )

parseGen1Box :: NameLength -> BoxCapacity -> Cursor -> (RawGen1Box, Cursor)
parseGen1Box nameLen boxCapacity cursor0 =
  let nameLenInt        = unNameLength nameLen
      boxCapInt         = unBoxCapacity boxCapacity
      (count, cursor1)  = readByte cursor0
      entryCount        = fromIntegral count
      speciesListSize   = boxCapInt + 1
      (species, cursor2) = parseSpeciesList boxCapInt speciesListSize cursor1
      (members, cursor3)    = parseFixedArray entryCount boxCapInt
                             gen1BoxPokemonSize parseGen1BoxPokemon cursor2
      (otNames, cursor4) = parseFixedArray entryCount boxCapInt
                             nameLenInt (readBytes nameLenInt) cursor3
      (nicknames, cursor5)   = parseFixedArray entryCount boxCapInt
                             nameLenInt (readBytes nameLenInt) cursor4
  in ( RawGen1Box
        { rawGen1BoxCount   = count
        , rawGen1BoxSpecies = species
        , rawGen1BoxMembers    = members
        , rawGen1BoxOTNames = otNames
        , rawGen1BoxNicknames   = nicknames
        }
     , cursor5
     )


-- ── Fixed-Array Helpers ────────────────────────────────────────

-- | Read species bytes until 0xFF or capacity reached, then
-- advance past the full fixed-size species list region.
parseSpeciesList :: Int -> Int -> Cursor -> ([InternalIndex], Cursor)
parseSpeciesList capacity totalSize cursor0 =
  (collectSpecies capacity cursor0, skip totalSize cursor0)
  where
    collectSpecies 0 _ = []
    collectSpecies remaining cursor =
      let (byte, nextCursor) = readByte cursor
      in if byte == 0xFF
        then []
        else InternalIndex byte : collectSpecies (remaining - 1) nextCursor

-- | Parse count entries using the given parser, then skip past
-- the remaining unused slots to maintain correct cursor position.
parseFixedArray :: Int -> Int -> Int -> (Cursor -> (a, Cursor)) -> Cursor -> ([a], Cursor)
parseFixedArray count capacity slotSize parser cursor0 =
  let (entries, cursorAfterEntries) = readEntries count cursor0
      unusedSlots = capacity - count
  in (entries, skip (unusedSlots * slotSize) cursorAfterEntries)
  where
    readEntries 0 cursor = ([], cursor)
    readEntries remaining cursor =
      let (entry, nextCursor)  = parser cursor
          (rest, finalCursor) = readEntries (remaining - 1) nextCursor
      in (entry : rest, finalCursor)


-- ── Box Bank Parsers ─────────────────────────────────────────

parseBoxBanks :: NameLength -> BoxCapacity -> ByteString -> [BoxBankInfo] -> ([RawGen1Box], [RawBankValidity])
parseBoxBanks nameLen boxCapacity bytes banks =
  let results = map (parseBoxBank nameLen boxCapacity bytes) banks
  in (concatMap fst results, map snd results)

parseBoxBank :: NameLength -> BoxCapacity -> ByteString -> BoxBankInfo -> ([RawGen1Box], RawBankValidity)
parseBoxBank nameLen boxCapacity bytes bank =
  let cursor = mkCursor bytes

      boxes =
        [ fst (parseGen1Box nameLen boxCapacity
                (seekTo (bankStartOffset bank + boxIndex * bankBoxDataSize bank) cursor))
        | boxIndex <- [0 .. bankBoxCount bank - 1]
        ]

      storedBankChecksum = ByteString.index bytes (bankAllChecksum bank)
      calculatedBankChecksum = calculateGen1Checksum bytes
                                 (bankStartOffset bank) (bankAllChecksum bank - 1)
      bankValid = storedBankChecksum == calculatedBankChecksum

      boxValid =
        [ let boxOffset = bankStartOffset bank + boxIndex * bankBoxDataSize bank
              stored     = ByteString.index bytes (bankBoxChecksums bank + boxIndex)
              calculated = calculateGen1Checksum bytes
                             boxOffset (boxOffset + bankBoxDataSize bank - 1)
          in stored == calculated
        | boxIndex <- [0 .. bankBoxCount bank - 1]
        ]

  in (boxes, RawBankValidity { bankChecksumValid = bankValid, boxChecksumsValid = boxValid })
