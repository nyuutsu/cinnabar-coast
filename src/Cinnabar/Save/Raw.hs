{-# LANGUAGE OverloadedStrings #-}

-- | Top-level save file types and parser.
--
-- Takes a CartridgeLayout and raw bytes, validates the file, and
-- dispatches to the generation-specific parser. Currently supports
-- Gen 1; Gen 2 returns UnimplementedGen.

module Cinnabar.Save.Raw
  ( -- * Errors (re-exported from Binary)
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

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8, Word16)

import Cinnabar.Binary
  ( SaveError (..), Parser, runParser
  , readByte, readByteAt, readWord16BE, readBytes
  , seek, skip
  )
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
  , gen1HoFRecordCount, gen1HoFSlotsPerRecord, gen1HoFEntrySize
  )
import Cinnabar.Types (Gen (..), InternalIndex (..))


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
  , rawGen1Checksum            :: !Word8
  , rawGen1CalculatedChecksum :: !Word8
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
  { bankStoredChecksum     :: !Word8
  , bankCalculatedChecksum :: !Word8
  , boxChecksumPairs       :: ![(Word8, Word8)]  -- (stored, calculated) per box
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
        RawGen1Save <$> runParser (parseGen1Save layout offsets bytes) bytes
    | otherwise ->
        Left (UnsupportedLayout "Gen 1 game with Gen 2 offsets")
  Gen2 -> Left (UnimplementedGen Gen2)


-- ── Gen 1 Save Parsing ────────────────────────────────────────

parseGen1Save :: CartridgeLayout -> Gen1SaveOffsets -> ByteString -> Parser RawGen1SaveFile
parseGen1Save layout offsets bytes = do
  let nameLen     = layoutNameLen layout
      nameLenInt  = unNameLength nameLen
      boxCapacity = layoutBoxCapacity layout

  seek (g1PlayerName offsets)
  playerName <- readBytes nameLenInt

  seek (g1RivalName offsets)
  rivalName <- readBytes nameLenInt

  seek (g1PartyData offsets)
  party <- parseGen1Party nameLen

  seek (g1CurrentBox offsets)
  currentBox <- parseGen1Box nameLen boxCapacity

  seek (g1PokedexOwned offsets)
  pokedexOwned <- readBytes 19

  seek (g1PokedexSeen offsets)
  pokedexSeen <- readBytes 19

  seek (g1BagItems offsets)
  bagItems <- parseItemList

  seek (g1BoxItems offsets)
  boxItems <- parseItemList

  seek (g1Money offsets)
  money <- readBytes 3

  seek (g1CasinoCoins offsets)
  casinoCoins <- readBytes 2

  seek (g1Badges offsets)
  badges <- readByte

  seek (g1PlayerID offsets)
  playerID <- readWord16BE

  seek (g1Options offsets)
  options <- readByte

  seek (g1CurrentBoxNumber offsets)
  boxNumber <- readByte

  seek (g1HoFCount offsets)
  hofCount <- readByte

  seek (g1PlayTime offsets)
  playTime <- parseRawPlayTime

  seek (g1PikachuHappiness offsets)
  pikachuHappiness <- readByte

  seek (g1PikachuMood offsets)
  pikachuMood <- readByte

  seek (g1SurfingHiScore offsets)
  surfingHiScore <- readBytes 2

  seek (g1PrinterSettings offsets)
  printerSettings <- readByte

  daycare        <- parseRawDaycare nameLen offsets
  progress       <- parseRawProgress offsets
  playerPosition <- parseRawPlayerPosition offsets
  safari         <- parseRawSafari offsets
  fossil         <- parseRawFossil offsets
  transient      <- parseRawTransient offsets

  seek (g1HallOfFame offsets)
  hallOfFame <- parseHallOfFame nameLen

  (pcBoxes, boxBankValidity) <- lift (parseBoxBanks nameLen boxCapacity bytes (g1BoxBanks offsets))

  seek (g1Checksum offsets)
  storedChecksum <- readByte
  let calculatedChecksum = calculateGen1Checksum bytes
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)

  pure RawGen1SaveFile
    { rawGen1Bytes            = bytes
    , rawGen1Layout           = layout
    , rawGen1PlayerName       = playerName
    , rawGen1RivalName        = rivalName
    , rawGen1Party            = party
    , rawGen1CurrentBox       = currentBox
    , rawGen1Checksum            = storedChecksum
    , rawGen1CalculatedChecksum = calculatedChecksum
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
parseItemList :: Parser [RawItemEntry]
parseItemList = do
  count <- readByte
  items <- replicateM (fromIntegral count) parseItemEntry
  _ <- readByte  -- terminator
  pure items
  where
    parseItemEntry :: Parser RawItemEntry
    parseItemEntry = do
      itemByte     <- readByte
      quantityByte <- readByte
      pure RawItemEntry { rawItemId = itemByte, rawItemQuantity = quantityByte }


-- ── Play Time / Daycare Parsers ─────────────────────────────

parseRawPlayTime :: Parser RawPlayTime
parseRawPlayTime = do
  hours   <- readByte
  maxed   <- readByte
  minutes <- readByte
  seconds <- readByte
  frames  <- readByte
  pure RawPlayTime
    { rawPlayHours   = hours
    , rawPlayMaxed   = maxed
    , rawPlayMinutes = minutes
    , rawPlaySeconds = seconds
    , rawPlayFrames  = frames
    }

parseRawDaycare :: NameLength -> Gen1SaveOffsets -> Parser RawDaycare
parseRawDaycare nameLen offsets = do
  let nameLenInt = unNameLength nameLen
  seek (g1DaycareInUse offsets)
  inUse <- readByte
  seek (g1DaycarePokemon offsets)
  speciesByte <- readByte
  seek (g1DaycareNickname offsets)
  nickname <- readBytes nameLenInt
  seek (g1DaycareOTName offsets)
  otName <- readBytes nameLenInt
  pure RawDaycare
    { rawDaycareInUse    = inUse
    , rawDaycarePokemon      = InternalIndex speciesByte
    , rawDaycareNickname = nickname
    , rawDaycareOTName   = otName
    }

parseRawProgress :: Gen1SaveOffsets -> Parser RawProgressFlags
parseRawProgress offsets = do
  seek (g1EventFlags offsets)
  eventFlags <- readBytes 320
  seek (g1ToggleFlags offsets)
  toggleFlags <- readBytes 32
  seek (g1MapScripts offsets)
  mapScripts <- readBytes 256
  seek (g1DefeatedGyms offsets)
  defeatedGyms <- readByte
  seek (g1PlayerStarter offsets)
  playerStarter <- readByte
  seek (g1RivalStarter offsets)
  rivalStarter <- readByte
  seek (g1TownsVisited offsets)
  townsVisited <- readBytes 2
  seek (g1MovementStatus offsets)
  movementStatus <- readByte
  seek (g1VarFlags1 offsets)
  varFlags1 <- readByte
  seek (g1VarFlags2 offsets)
  varFlags2 <- readByte
  seek (g1VarFlags3 offsets)
  varFlags3 <- readByte
  seek (g1VarFlags4 offsets)
  varFlags4 <- readByte
  seek (g1VarFlags5 offsets)
  varFlags5 <- readByte
  seek (g1VarFlags6 offsets)
  varFlags6 <- readByte
  seek (g1VarFlags7 offsets)
  varFlags7 <- readByte
  seek (g1VarFlags8 offsets)
  varFlags8 <- readByte
  seek (g1DefeatedLorelei offsets)
  defeatedLorelei <- readBytes 2
  seek (g1InGameTrades offsets)
  inGameTrades <- readBytes 2
  seek (g1HiddenItems offsets)
  hiddenItems <- readBytes 14
  seek (g1HiddenCoins offsets)
  hiddenCoins <- readBytes 2
  seek (g1CurrentMap offsets)
  currentMap <- readByte
  pure RawProgressFlags
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

parseRawPlayerPosition :: Gen1SaveOffsets -> Parser RawPlayerPosition
parseRawPlayerPosition offsets = do
  seek (g1PlayerY offsets)
  playerY <- readByte
  seek (g1PlayerX offsets)
  playerX <- readByte
  seek (g1LastMap offsets)
  lastMap <- readByte
  seek (g1LastBlackoutMap offsets)
  blackoutMap <- readByte
  seek (g1DestinationMap offsets)
  destinationMap <- readByte
  pure RawPlayerPosition
    { rawPlayerY         = playerY
    , rawPlayerX         = playerX
    , rawLastMap         = lastMap
    , rawLastBlackoutMap = blackoutMap
    , rawDestinationMap  = destinationMap
    }

parseRawSafari :: Gen1SaveOffsets -> Parser RawSafariState
parseRawSafari offsets = do
  seek (g1SafariSteps offsets)
  safariSteps <- readWord16BE
  seek (g1SafariBallCount offsets)
  ballCount <- readByte
  seek (g1SafariGameOver offsets)
  gameOver <- readByte
  pure RawSafariState
    { rawSafariSteps     = safariSteps
    , rawSafariBallCount = ballCount
    , rawSafariGameOver  = gameOver
    }

parseRawFossil :: Gen1SaveOffsets -> Parser RawFossilState
parseRawFossil offsets = do
  seek (g1FossilItem offsets)
  itemGiven <- readByte
  seek (g1FossilResult offsets)
  fossilResult <- readBytes 3
  pure RawFossilState
    { rawFossilItemGiven = itemGiven
    , rawFossilResult    = fossilResult
    }

parseRawTransient :: Gen1SaveOffsets -> Parser RawTransientState
parseRawTransient offsets = do
  seek (g1LetterDelay offsets)
  letterDelay <- readByte
  seek (g1MusicId offsets)
  musicId <- readByte
  seek (g1MusicBank offsets)
  musicBank <- readByte
  seek (g1ContrastId offsets)
  contrastId <- readByte
  seek (g1EnemyTrainerClass offsets)
  trainerClass <- readByte
  seek (g1BoulderSpriteIndex offsets)
  boulderSprite <- readByte
  seek (g1DungeonWarpDest offsets)
  dungeonDest <- readByte
  seek (g1DungeonWarpUsed offsets)
  dungeonUsed <- readByte
  seek (g1WarpedFromWarp offsets)
  warpedWarp <- readByte
  seek (g1WarpedFromMap offsets)
  warpedMap <- readByte
  seek (g1CardKeyDoorY offsets)
  cardKeyY <- readByte
  seek (g1CardKeyDoorX offsets)
  cardKeyX <- readByte
  seek (g1TrashCanLock1 offsets)
  trashLock1 <- readByte
  seek (g1TrashCanLock2 offsets)
  trashLock2 <- readByte
  seek (g1CurrentMapScript offsets)
  mapScript <- readByte
  pure RawTransientState
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

parseHallOfFame :: NameLength -> Parser [RawGen1HoFRecord]
parseHallOfFame nameLen = replicateM gen1HoFRecordCount (parseHoFRecord nameLen)

parseHoFRecord :: NameLength -> Parser RawGen1HoFRecord
parseHoFRecord nameLen = do
  entries <- replicateM gen1HoFSlotsPerRecord (parseHoFEntry nameLen)
  pure RawGen1HoFRecord { rawGen1HoFEntries = entries }

parseHoFEntry :: NameLength -> Parser RawGen1HoFEntry
parseHoFEntry nameLen = do
  let nameLenInt = unNameLength nameLen
      paddingLen = gen1HoFEntrySize - 2 - nameLenInt
  speciesByte <- readByte
  levelByte   <- readByte
  nickname    <- readBytes nameLenInt
  padding     <- readBytes paddingLen
  pure RawGen1HoFEntry
    { rawGen1HoFSpecies  = InternalIndex speciesByte
    , rawGen1HoFLevel    = levelByte
    , rawGen1HoFNickname = nickname
    , rawGen1HoFPadding  = padding
    }


-- ── Container Parsers ──────────────────────────────────────────

parseGen1Party :: NameLength -> Parser RawGen1Party
parseGen1Party nameLen = do
  let nameLenInt      = unNameLength nameLen
      speciesListSize = gen1PartyCapacity + 1
  count   <- readByte
  let entryCount = fromIntegral count
  species <- parseSpeciesList gen1PartyCapacity speciesListSize
  members <- parseFixedArray entryCount gen1PartyCapacity
               gen1PartyPokemonSize parseGen1PartyPokemon
  otNames <- parseFixedArray entryCount gen1PartyCapacity
               nameLenInt (readBytes nameLenInt)
  nicknames <- parseFixedArray entryCount gen1PartyCapacity
                 nameLenInt (readBytes nameLenInt)
  pure RawGen1Party
    { rawGen1PartyCount     = count
    , rawGen1PartySpecies   = species
    , rawGen1PartyMembers   = members
    , rawGen1PartyOTNames   = otNames
    , rawGen1PartyNicknames = nicknames
    }

parseGen1Box :: NameLength -> BoxCapacity -> Parser RawGen1Box
parseGen1Box nameLen boxCapacity = do
  let nameLenInt = unNameLength nameLen
      boxCapInt  = unBoxCapacity boxCapacity
      speciesListSize = boxCapInt + 1
  count   <- readByte
  let entryCount = fromIntegral count
  species <- parseSpeciesList boxCapInt speciesListSize
  members <- parseFixedArray entryCount boxCapInt
               gen1BoxPokemonSize parseGen1BoxPokemon
  otNames <- parseFixedArray entryCount boxCapInt
               nameLenInt (readBytes nameLenInt)
  nicknames <- parseFixedArray entryCount boxCapInt
                 nameLenInt (readBytes nameLenInt)
  pure RawGen1Box
    { rawGen1BoxCount     = count
    , rawGen1BoxSpecies   = species
    , rawGen1BoxMembers   = members
    , rawGen1BoxOTNames   = otNames
    , rawGen1BoxNicknames = nicknames
    }


-- ── Fixed-Array Helpers ────────────────────────────────────────

-- | Read species bytes until 0xFF or capacity reached, then
-- advance past the full fixed-size species list region.
parseSpeciesList :: Int -> Int -> Parser [InternalIndex]
parseSpeciesList capacity totalSize = do
  species <- collectSpecies capacity
  -- The species list region is fixed-size; we already read some bytes
  -- collecting species (up to capacity + 1 for the terminator), but
  -- the simple approach is: seek back and skip the whole region.
  -- Instead, skip the remaining bytes we didn't consume.
  -- We consumed: length species bytes (the species) + 1 if terminated by 0xFF
  -- (or 0 if we hit capacity). Total region is totalSize bytes.
  -- Simpler: just skip the difference.
  let consumed = length species + if length species < capacity then 1 else 0
  skip (totalSize - consumed)
  pure species
  where
    collectSpecies :: Int -> Parser [InternalIndex]
    collectSpecies 0 = pure []
    collectSpecies remaining = do
      byte <- readByte
      if byte == 0xFF
        then pure []
        else do
          rest <- collectSpecies (remaining - 1)
          pure (InternalIndex byte : rest)

-- | Parse count entries using the given parser, then skip past
-- the remaining unused slots to maintain correct cursor position.
parseFixedArray :: Int -> Int -> Int -> Parser a -> Parser [a]
parseFixedArray count capacity slotSize parser = do
  entries <- replicateM count parser
  let unusedSlots = capacity - count
  skip (unusedSlots * slotSize)
  pure entries


-- ── Box Bank Parsers ─────────────────────────────────────────

parseBoxBanks :: NameLength -> BoxCapacity -> ByteString -> [BoxBankInfo] -> Either SaveError ([RawGen1Box], [RawBankValidity])
parseBoxBanks nameLen boxCapacity bytes banks = do
  results <- mapM (parseBoxBank nameLen boxCapacity bytes) banks
  pure (concatMap fst results, map snd results)

parseBoxBank :: NameLength -> BoxCapacity -> ByteString -> BoxBankInfo -> Either SaveError ([RawGen1Box], RawBankValidity)
parseBoxBank nameLen boxCapacity bytes bank = do
  boxes <- sequence
    [ runParser (do seek (bankStartOffset bank + boxIndex * bankBoxDataSize bank)
                    parseGen1Box nameLen boxCapacity
                ) bytes
    | boxIndex <- [0 .. bankBoxCount bank - 1]
    ]

  storedBankChecksum <- readByteAt (bankAllChecksum bank) bytes
  let calculatedBankChecksum = calculateGen1Checksum bytes
                                 (bankStartOffset bank) (bankAllChecksum bank - 1)

  boxPairs <- sequence
    [ do stored <- readByteAt (bankBoxChecksums bank + boxIndex) bytes
         let boxOffset  = bankStartOffset bank + boxIndex * bankBoxDataSize bank
             calculated = calculateGen1Checksum bytes
                            boxOffset (boxOffset + bankBoxDataSize bank - 1)
         pure (stored, calculated)
    | boxIndex <- [0 .. bankBoxCount bank - 1]
    ]

  pure ( boxes
       , RawBankValidity
           { bankStoredChecksum     = storedBankChecksum
           , bankCalculatedChecksum = calculatedBankChecksum
           , boxChecksumPairs       = boxPairs
           }
       )
