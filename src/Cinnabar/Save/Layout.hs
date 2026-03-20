{-# LANGUAGE OverloadedStrings #-}

-- | Binary layout configuration for Gen 1/2 save files.
--
-- Layout is data, not code — adding a new game+region means adding
-- a value, not modifying the parser. Each CartridgeLayout describes
-- where structures live in SRAM for a given game and region.

module Cinnabar.Save.Layout
  ( -- * Game identification
    GameVariant (..)
  , SaveRegion (..)

    -- * Layout configuration
  , CartridgeLayout (..)
  , SaveOffsets (..)
  , Gen1SaveOffsets (..)
  , Gen2SaveOffsets (..)
  , BoxBankInfo (..)

    -- * Struct sizes
  , gen1PartyCapacity
  , gen1PartyMonSize
  , gen1BoxMonSize

    -- * Lookup
  , cartridgeLayout
  ) where

import Data.Text (Text)

import Cinnabar.Types (Gen (..))


-- ── Game Variant ────────────────────────────────────────────────

data GameVariant = RedBlue | Yellow | GoldSilver | Crystal
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Save Region ─────────────────────────────────────────────────

-- | Binary layout region. Determines name field byte lengths and
-- save structure offsets. Not the same as Language — text encoding
-- is handled separately by TextCodec.
data SaveRegion = RegionJapanese | RegionWestern | RegionKorean
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Cartridge Layout ────────────────────────────────────────────

data CartridgeLayout = CartridgeLayout
  { layoutGen         :: !Gen
  , layoutGame        :: !GameVariant
  , layoutRegion      :: !SaveRegion
  , layoutNameLen     :: !Int
  , layoutOffsets     :: !SaveOffsets
  , layoutBoxCount    :: !Int
  , layoutBoxCapacity :: !Int
  } deriving (Show)


-- ── Save Offsets ────────────────────────────────────────────────

-- | Gen 1 and Gen 2 have different checksum structures. Sum type,
-- not a record with Maybe fields.
data SaveOffsets
  = Gen1Offsets !Gen1SaveOffsets
  | Gen2Offsets !Gen2SaveOffsets
  deriving (Show)

data Gen1SaveOffsets = Gen1SaveOffsets
  { g1PlayerName        :: !Int
  , g1RivalName         :: !Int
  , g1PartyData         :: !Int
  , g1CurrentBox        :: !Int
  , g1Checksum          :: !Int
  , g1ChecksumStart     :: !Int
  , g1ChecksumEnd       :: !Int
  , g1BoxBanks          :: ![BoxBankInfo]
  , g1PokedexOwned      :: !Int   -- 19 bytes, bit-packed
  , g1PokedexSeen       :: !Int   -- 19 bytes, bit-packed
  , g1BagItems          :: !Int
  , g1Money             :: !Int   -- 3 bytes BCD
  , g1Options           :: !Int   -- 1 byte
  , g1Badges            :: !Int   -- 1 byte bitfield
  , g1PlayerID          :: !Int   -- 2 bytes big-endian
  , g1PikachuFriendship :: !Int   -- 1 byte (Yellow only; unused in R/B)
  , g1BoxItems          :: !Int
  , g1CurrentBoxNumber  :: !Int   -- 1 byte
  , g1HoFCount          :: !Int   -- 1 byte
  , g1CasinoCoins       :: !Int   -- 2 bytes BCD
  , g1PlayTime          :: !Int   -- 5 consecutive bytes
  , g1DaycareInUse      :: !Int   -- 1 byte
  , g1DaycareMon        :: !Int   -- 1 byte (internal species index)
  } deriving (Show)

data Gen2SaveOffsets = Gen2SaveOffsets
  { g2PlayerName     :: !Int
  , g2RivalName      :: !Int
  , g2PartyData      :: !Int
  , g2CurrentBox     :: !Int
  , g2Checksum1      :: !Int
  , g2Checksum1Start :: !Int
  , g2Checksum1End   :: !Int
  , g2Checksum2      :: !Int
  , g2Checksum2Start :: !Int
  , g2Checksum2End   :: !Int
  , g2BoxBanks       :: ![BoxBankInfo]
  } deriving (Show)

data BoxBankInfo = BoxBankInfo
  { bankStartOffset  :: !Int
  , bankBoxCount     :: !Int
  , bankBoxDataSize  :: !Int
  , bankAllChecksum  :: !Int   -- offset of the bank-wide checksum byte
  , bankBoxChecksums :: !Int   -- offset of the first per-box checksum byte (consecutive)
  } deriving (Show)


-- ── Struct Sizes ──────────────────────────────────────────────────

-- | Gen 1 container and struct sizes.
--
-- Source: Bulbapedia, "Save data structure (Generation I)"
-- Party: always 6 capacity, 44-byte party structs, 33-byte box structs
-- Box capacity varies by region: 20 (Western), 30 (Japanese)

gen1PartyCapacity :: Int
gen1PartyCapacity = 6

gen1PartyMonSize :: Int
gen1PartyMonSize = 44

gen1BoxMonSize :: Int
gen1BoxMonSize = 33


-- ── Layout Lookup ───────────────────────────────────────────────

cartridgeLayout :: GameVariant -> SaveRegion -> Either Text CartridgeLayout
cartridgeLayout RedBlue RegionWestern = Right (westernGen1Layout RedBlue)
cartridgeLayout Yellow  RegionWestern = Right (westernGen1Layout Yellow)
cartridgeLayout game    region        =
  Left $ "layout not yet implemented: " <> renderRegion region <> " " <> renderVariant game


-- ── Internal ────────────────────────────────────────────────────

westernGen1Layout :: GameVariant -> CartridgeLayout
westernGen1Layout game = CartridgeLayout
  { layoutGen         = Gen1
  , layoutGame        = game
  , layoutRegion      = RegionWestern
  , layoutNameLen     = 11
  , layoutOffsets     = Gen1Offsets westernGen1Offsets
  , layoutBoxCount    = 12
  , layoutBoxCapacity = 20
  }

-- | Western Gen 1 save file offsets. Shared across Red, Blue, and Yellow.
--
-- Source: Bulbapedia, "Save data structure (Generation I)"
-- https://bulbapedia.bulbagarden.net/wiki/Save_data_structure_(Generation_I)
--
-- Player name:        Bank 1, 0x2598 (11 bytes)
-- Pokedex owned:      Bank 1, 0x25A3 (19 bytes, bit-packed)
-- Pokedex seen:       Bank 1, 0x25B6 (19 bytes, bit-packed)
-- Bag items:          Bank 1, 0x25C9 (count + pairs + 0xFF)
-- Money:              Bank 1, 0x25F3 (3 bytes BCD)
-- Rival name:         Bank 1, 0x25F6 (11 bytes)
-- Options:            Bank 1, 0x2601 (1 byte)
-- Badges:             Bank 1, 0x2602 (1 byte bitfield)
-- Player ID:          Bank 1, 0x2605 (2 bytes big-endian)
-- Pikachu friendship: Bank 1, 0x271C (1 byte, Yellow only)
-- PC box items:       Bank 1, 0x27E6 (count + pairs + 0xFF)
-- Current box number: Bank 1, 0x284C (1 byte)
-- Hall of Fame count: Bank 1, 0x284E (1 byte)
-- Casino coins:       Bank 1, 0x2850 (2 bytes BCD)
-- Play time:          Bank 1, 0x2CED (5 bytes: hours, maxed, min, sec, frames)
-- Daycare in use:     Bank 1, 0x2CF4 (1 byte)
-- Daycare mon:        Bank 1, 0x2D0B (1 byte, internal species index)
-- Party data:         Bank 1, 0x2F2C (0x194 bytes)
-- Current box:        Bank 1, 0x30C0 (0x462 bytes)
-- Checksum:           Bank 1, 0x3523 (1 byte)
-- Checksum range:     0x2598-0x3522 (complement of byte sum)
-- Box banks:          Bank 2 at 0x4000, Bank 3 at 0x6000 (6 boxes each, 0x462 per box)
-- Bank 2 checksum:    0x5A4C (1 byte, complement of byte sum over 0x4000-0x5A4B)
-- Bank 2 box checks:  0x5A4D (6 bytes, one per box)
-- Bank 3 checksum:    0x7A4C (1 byte, complement of byte sum over 0x6000-0x7A4B)
-- Bank 3 box checks:  0x7A4D (6 bytes, one per box)
westernGen1Offsets :: Gen1SaveOffsets
westernGen1Offsets = Gen1SaveOffsets
  { g1PlayerName        = 0x2598
  , g1RivalName         = 0x25F6
  , g1PartyData         = 0x2F2C
  , g1CurrentBox        = 0x30C0
  , g1Checksum          = 0x3523
  , g1ChecksumStart     = 0x2598
  , g1ChecksumEnd       = 0x3522
  , g1BoxBanks          =
      [ BoxBankInfo { bankStartOffset = 0x4000, bankBoxCount = 6, bankBoxDataSize = 1122
                    , bankAllChecksum = 0x5A4C, bankBoxChecksums = 0x5A4D }
      , BoxBankInfo { bankStartOffset = 0x6000, bankBoxCount = 6, bankBoxDataSize = 1122
                    , bankAllChecksum = 0x7A4C, bankBoxChecksums = 0x7A4D }
      ]
  , g1PokedexOwned      = 0x25A3
  , g1PokedexSeen       = 0x25B6
  , g1BagItems          = 0x25C9
  , g1Money             = 0x25F3
  , g1Options           = 0x2601
  , g1Badges            = 0x2602
  , g1PlayerID          = 0x2605
  , g1PikachuFriendship = 0x271C
  , g1BoxItems          = 0x27E6
  , g1CurrentBoxNumber  = 0x284C
  , g1HoFCount          = 0x284E
  , g1CasinoCoins       = 0x2850
  , g1PlayTime          = 0x2CED
  , g1DaycareInUse      = 0x2CF4
  , g1DaycareMon        = 0x2D0B
  }

renderVariant :: GameVariant -> Text
renderVariant RedBlue    = "Red/Blue"
renderVariant Yellow     = "Yellow"
renderVariant GoldSilver = "Gold/Silver"
renderVariant Crystal    = "Crystal"

renderRegion :: SaveRegion -> Text
renderRegion RegionJapanese = "Japanese"
renderRegion RegionWestern  = "Western"
renderRegion RegionKorean   = "Korean"
