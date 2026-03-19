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
  { g1PlayerName    :: !Int
  , g1RivalName     :: !Int
  , g1PartyData     :: !Int
  , g1CurrentBox    :: !Int
  , g1Checksum      :: !Int
  , g1ChecksumStart :: !Int
  , g1ChecksumEnd   :: !Int
  , g1BoxBanks      :: ![BoxBankInfo]
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
  { bankStartOffset :: !Int
  , bankBoxCount    :: !Int
  , bankBoxDataSize :: !Int
  } deriving (Show)


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

-- | Shared across Red, Blue, and Yellow.
westernGen1Offsets :: Gen1SaveOffsets
westernGen1Offsets = Gen1SaveOffsets
  { g1PlayerName    = 0x2598
  , g1RivalName     = 0x25A3
  , g1PartyData     = 0x2F2C
  , g1CurrentBox    = 0x30C0
  , g1Checksum      = 0x3523
  , g1ChecksumStart = 0x2598
  , g1ChecksumEnd   = 0x3522
  , g1BoxBanks      =
      [ BoxBankInfo { bankStartOffset = 0x4000, bankBoxCount = 6, bankBoxDataSize = 1122 }
      , BoxBankInfo { bankStartOffset = 0x6000, bankBoxCount = 6, bankBoxDataSize = 1122 }
      ]
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
