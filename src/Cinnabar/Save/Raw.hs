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

    -- * Container types
  , RawGen1Party (..)
  , RawGen1Box (..)

    -- * Parser
  , parseRawSave
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import Data.Word (Word8)

import Cinnabar.Binary (Cursor, mkCursor, readByte, readBytes, seekTo, skip)
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
  { rawGen1Bytes         :: !ByteString
  , rawGen1Layout        :: !CartridgeLayout
  , rawGen1PlayerName    :: !ByteString
  , rawGen1RivalName     :: !ByteString
  , rawGen1Party         :: !RawGen1Party
  , rawGen1CurrentBox    :: !RawGen1Box
  , rawGen1Checksum      :: !Word8
  , rawGen1ChecksumValid :: !Bool
  }

-- | Stub -- will be fleshed out when Gen 2 parsing is implemented.
data RawGen2SaveFile = RawGen2SaveFile


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

      storedChecksum     = ByteString.index bytes (g1Checksum offsets)
      calculatedChecksum = calculateGen1Checksum bytes
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)

  in RawGen1SaveFile
      { rawGen1Bytes         = bytes
      , rawGen1Layout        = layout
      , rawGen1PlayerName    = playerName
      , rawGen1RivalName     = rivalName
      , rawGen1Party         = party
      , rawGen1CurrentBox    = currentBox
      , rawGen1Checksum      = storedChecksum
      , rawGen1ChecksumValid = storedChecksum == calculatedChecksum
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
