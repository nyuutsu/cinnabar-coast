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
  , RawPlayTime (..)
  , RawDaycare (..)

    -- * Container types
  , RawGen1Party (..)
  , RawGen1Box (..)

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
  ( RawGen1PartyMon, RawGen1BoxMon
  , parseGen1PartyMon, parseGen1BoxMon
  )
import Cinnabar.Save.Layout
  ( CartridgeLayout (..)
  , SaveOffsets (..)
  , Gen1SaveOffsets (..)
  , gen1PartyCapacity, gen1PartyMonSize, gen1BoxMonSize
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
  , rawGen1BagItems         :: ![(Word8, Word8)]
  , rawGen1BoxItems         :: ![(Word8, Word8)]
  , rawGen1Money            :: !ByteString
  , rawGen1CasinoCoins      :: !ByteString
  , rawGen1Badges           :: !Word8
  , rawGen1PlayerID         :: !Word16
  , rawGen1Options          :: !Word8
  , rawGen1CurrentBoxNum    :: !Word8
  , rawGen1HoFCount         :: !Word8
  , rawGen1PlayTime         :: !RawPlayTime
  , rawGen1PikachuFriend    :: !Word8
  , rawGen1Daycare          :: !RawDaycare
  }

-- | Stub -- will be fleshed out when Gen 2 parsing is implemented.
data RawGen2SaveFile = RawGen2SaveFile


-- ── Sub-Record Types ─────────────────────────────────────────

data RawPlayTime = RawPlayTime
  { rawPlayHours   :: !Word8
  , rawPlayMaxed   :: !Word8   -- nonzero if the timer has capped
  , rawPlayMinutes :: !Word8
  , rawPlaySeconds :: !Word8
  , rawPlayFrames  :: !Word8   -- 1/60th second
  } deriving (Eq, Show)

data RawDaycare = RawDaycare
  { rawDaycareInUse :: !Word8            -- 0 = empty, nonzero = occupied
  , rawDaycareMon   :: !InternalIndex    -- species if in use
  } deriving (Eq, Show)


-- ── Container Types ────────────────────────────────────────────

data RawGen1Party = RawGen1Party
  { rawGen1PartyCount   :: !Word8
  , rawGen1PartySpecies :: ![InternalIndex]
  , rawGen1PartyMons    :: ![RawGen1PartyMon]
  , rawGen1PartyOTNames :: ![ByteString]
  , rawGen1PartyNicks   :: ![ByteString]
  } deriving (Eq, Show)

data RawGen1Box = RawGen1Box
  { rawGen1BoxCount   :: !Word8
  , rawGen1BoxSpecies :: ![InternalIndex]
  , rawGen1BoxMons    :: ![RawGen1BoxMon]
  , rawGen1BoxOTNames :: ![ByteString]
  , rawGen1BoxNicks   :: ![ByteString]
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

      (playerName, _) = readBytes nameLen (seekTo (g1PlayerName offsets) cursor)
      (rivalName, _)  = readBytes nameLen (seekTo (g1RivalName offsets) cursor)
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
      (pikachuFriend, _) = readByte (seekTo (g1PikachuFriendship offsets) cursor)
      daycare           = parseRawDaycare offsets cursor

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
      , rawGen1PikachuFriend    = pikachuFriend
      , rawGen1Daycare          = daycare
      }


-- ── Item List Parser ─────────────────────────────────────────

-- | Parse a Gen 1 item list: 1 byte count, count x (item ID, quantity)
-- pairs, then a 0xFF terminator.
parseItemList :: Cursor -> ([(Word8, Word8)], Cursor)
parseItemList cursor0 =
  let (count, cursor1) = readByte cursor0
      itemCount        = fromIntegral count :: Int
      (items, cursor2) = readItemPairs itemCount cursor1
      (_terminator, cursor3) = readByte cursor2
  in (items, cursor3)
  where
    readItemPairs :: Int -> Cursor -> ([(Word8, Word8)], Cursor)
    readItemPairs 0 cursor = ([], cursor)
    readItemPairs remaining cursor =
      let (itemId, cursor1)   = readByte cursor
          (quantity, cursor2) = readByte cursor1
          (rest, cursor3)     = readItemPairs (remaining - 1) cursor2
      in ((itemId, quantity) : rest, cursor3)


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

parseRawDaycare :: Gen1SaveOffsets -> Cursor -> RawDaycare
parseRawDaycare offsets cursor =
  let (inUse, _)     = readByte (seekTo (g1DaycareInUse offsets) cursor)
      (speciesByte, _) = readByte (seekTo (g1DaycareMon offsets) cursor)
  in RawDaycare
      { rawDaycareInUse = inUse
      , rawDaycareMon   = InternalIndex speciesByte
      }


-- ── Container Parsers ──────────────────────────────────────────

parseGen1Party :: Int -> Cursor -> (RawGen1Party, Cursor)
parseGen1Party nameLen cursor0 =
  let (count, cursor1)  = readByte cursor0
      entryCount        = fromIntegral count
      speciesListSize   = gen1PartyCapacity + 1
      (species, cursor2) = parseSpeciesList gen1PartyCapacity speciesListSize cursor1
      (mons, cursor3)    = parseFixedArray entryCount gen1PartyCapacity
                             gen1PartyMonSize parseGen1PartyMon cursor2
      (otNames, cursor4) = parseFixedArray entryCount gen1PartyCapacity
                             nameLen (readBytes nameLen) cursor3
      (nicks, cursor5)   = parseFixedArray entryCount gen1PartyCapacity
                             nameLen (readBytes nameLen) cursor4
  in ( RawGen1Party
        { rawGen1PartyCount   = count
        , rawGen1PartySpecies = species
        , rawGen1PartyMons    = mons
        , rawGen1PartyOTNames = otNames
        , rawGen1PartyNicks   = nicks
        }
     , cursor5
     )

parseGen1Box :: Int -> Int -> Cursor -> (RawGen1Box, Cursor)
parseGen1Box nameLen boxCapacity cursor0 =
  let (count, cursor1)  = readByte cursor0
      entryCount        = fromIntegral count
      speciesListSize   = boxCapacity + 1
      (species, cursor2) = parseSpeciesList boxCapacity speciesListSize cursor1
      (mons, cursor3)    = parseFixedArray entryCount boxCapacity
                             gen1BoxMonSize parseGen1BoxMon cursor2
      (otNames, cursor4) = parseFixedArray entryCount boxCapacity
                             nameLen (readBytes nameLen) cursor3
      (nicks, cursor5)   = parseFixedArray entryCount boxCapacity
                             nameLen (readBytes nameLen) cursor4
  in ( RawGen1Box
        { rawGen1BoxCount   = count
        , rawGen1BoxSpecies = species
        , rawGen1BoxMons    = mons
        , rawGen1BoxOTNames = otNames
        , rawGen1BoxNicks   = nicks
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
