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
  , NameLength (..)
  , BoxCapacity (..)
  , SaveOffsets (..)
  , Gen1SaveOffsets (..)
  , Gen2SaveOffsets (..)
  , BoxBankInfo (..)

    -- * Struct sizes
  , gen1PartyCapacity
  , gen1PartyPokemonSize
  , gen1BoxPokemonSize
  , gen1BagCapacity
  , gen1PCItemCapacity
  , gen1HoFRecordCount
  , gen1HoFSlotsPerRecord
  , gen1HoFEntrySize
  , gen1HoFRecordSize

    -- * Lookup
  , cartridgeLayout
  ) where

import Data.Text (Text)

import Cinnabar.Binary (SaveOffset (..))
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


-- ── Layout Newtypes ──────────────────────────────────────────────

-- | Byte length of a name field (player, rival, OT, nickname).
-- Varies by region: 11 for Western, 6 for Japanese.
newtype NameLength = NameLength { unNameLength :: Int }
  deriving (Eq, Ord, Show)

-- | Maximum number of Pokemon a PC box can hold.
-- Varies by region: 20 for Western, 30 for Japanese.
newtype BoxCapacity = BoxCapacity { unBoxCapacity :: Int }
  deriving (Eq, Ord, Show)


-- ── Cartridge Layout ────────────────────────────────────────────

data CartridgeLayout = CartridgeLayout
  { layoutGen         :: !Gen
  , layoutGame        :: !GameVariant
  , layoutRegion      :: !SaveRegion
  , layoutNameLen     :: !NameLength
  , layoutOffsets     :: !SaveOffsets
  , layoutBoxCount    :: !Int
  , layoutBoxCapacity :: !BoxCapacity
  } deriving (Show)


-- ── Save Offsets ────────────────────────────────────────────────

-- | Gen 1 and Gen 2 have different checksum structures. Sum type,
-- not a record with Maybe fields.
data SaveOffsets
  = Gen1Offsets !Gen1SaveOffsets
  | Gen2Offsets !Gen2SaveOffsets
  deriving (Show)

data Gen1SaveOffsets = Gen1SaveOffsets
  { g1PlayerName        :: !SaveOffset
  , g1RivalName         :: !SaveOffset
  , g1PartyData         :: !SaveOffset
  , g1CurrentBox        :: !SaveOffset
  , g1Checksum          :: !SaveOffset
  , g1ChecksumStart     :: !SaveOffset
  , g1ChecksumEnd       :: !SaveOffset
  , g1BoxBanks          :: ![BoxBankInfo]
  , g1PokedexOwned      :: !SaveOffset   -- 19 bytes, bit-packed
  , g1PokedexSeen       :: !SaveOffset   -- 19 bytes, bit-packed
  , g1BagItems          :: !SaveOffset
  , g1Money             :: !SaveOffset   -- 3 bytes BCD
  , g1Options           :: !SaveOffset   -- 1 byte
  , g1Badges            :: !SaveOffset   -- 1 byte bitfield
  , g1PlayerID          :: !SaveOffset   -- 2 bytes big-endian
  , g1PikachuHappiness  :: !SaveOffset   -- 1 byte (Yellow only; unused in R/B)
  , g1PikachuMood       :: !SaveOffset   -- 1 byte (Yellow only; unused in R/B)
  , g1SurfingHiScore    :: !SaveOffset   -- 2 bytes little-endian BCD (Yellow only)
  , g1PrinterSettings   :: !SaveOffset   -- 1 byte (Yellow only)
  , g1BoxItems          :: !SaveOffset
  , g1CurrentBoxNumber  :: !SaveOffset   -- 1 byte
  , g1HoFCount          :: !SaveOffset   -- 1 byte
  , g1HallOfFame        :: !SaveOffset   -- Bank 0, 50 records × 6 entries × 16 bytes
  , g1CasinoCoins       :: !SaveOffset   -- 2 bytes BCD
  , g1PlayTime          :: !SaveOffset   -- 5 consecutive bytes
  , g1DaycareInUse      :: !SaveOffset   -- 1 byte
  , g1DaycarePokemon    :: !SaveOffset   -- 1 byte (internal species index)
  , g1EventFlags        :: !SaveOffset   -- 320 bytes (2560-bit flag array)
  , g1ToggleFlags       :: !SaveOffset   -- 32 bytes (256-bit flag array, toggleable objects)
  , g1MapScripts        :: !SaveOffset   -- 256 bytes (per-map script progress)
  , g1DefeatedGyms      :: !SaveOffset   -- 1 byte bitfield
  , g1PlayerStarter     :: !SaveOffset   -- 1 byte (internal species index)
  , g1RivalStarter      :: !SaveOffset   -- 1 byte (internal species index)
  , g1TownsVisited      :: !SaveOffset   -- 2 bytes bitfield
  , g1MovementStatus    :: !SaveOffset   -- 1 byte
  , g1VarFlags1         :: !SaveOffset   -- 1 byte
  , g1VarFlags2         :: !SaveOffset   -- 1 byte
  , g1VarFlags3         :: !SaveOffset   -- 1 byte
  , g1VarFlags4         :: !SaveOffset   -- 1 byte
  , g1VarFlags5         :: !SaveOffset   -- 1 byte
  , g1VarFlags6         :: !SaveOffset   -- 1 byte
  , g1InGameTrades      :: !SaveOffset   -- 2 bytes bitset
  , g1HiddenItems       :: !SaveOffset   -- 14 bytes
  , g1HiddenCoins       :: !SaveOffset   -- 2 bytes
  , g1CurrentMap        :: !SaveOffset   -- 1 byte
  , g1DaycareNickname   :: !SaveOffset   -- 11 bytes, raw text
  , g1DaycareOTName     :: !SaveOffset   -- 11 bytes, raw text
  , g1VarFlags7         :: !SaveOffset   -- 1 byte
  , g1VarFlags8         :: !SaveOffset   -- 1 byte
  , g1DefeatedLorelei   :: !SaveOffset   -- 2 bytes
  , g1PlayerY           :: !SaveOffset   -- 1 byte
  , g1PlayerX           :: !SaveOffset   -- 1 byte
  , g1LastMap           :: !SaveOffset   -- 1 byte
  , g1LastBlackoutMap   :: !SaveOffset   -- 1 byte
  , g1DestinationMap    :: !SaveOffset   -- 1 byte
  , g1SafariSteps       :: !SaveOffset   -- 2 bytes big-endian
  , g1SafariBallCount   :: !SaveOffset   -- 1 byte
  , g1SafariGameOver    :: !SaveOffset   -- 1 byte
  , g1FossilItem        :: !SaveOffset   -- 1 byte
  , g1FossilResult      :: !SaveOffset   -- 3 bytes
  , g1LetterDelay       :: !SaveOffset   -- 1 byte
  , g1MusicId           :: !SaveOffset   -- 1 byte
  , g1MusicBank         :: !SaveOffset   -- 1 byte
  , g1ContrastId        :: !SaveOffset   -- 1 byte
  , g1EnemyTrainerClass :: !SaveOffset   -- 1 byte
  , g1BoulderSpriteIndex :: !SaveOffset  -- 1 byte
  , g1DungeonWarpDest   :: !SaveOffset   -- 1 byte
  , g1DungeonWarpUsed   :: !SaveOffset   -- 1 byte
  , g1WarpedFromWarp    :: !SaveOffset   -- 1 byte
  , g1WarpedFromMap     :: !SaveOffset   -- 1 byte
  , g1CardKeyDoorY      :: !SaveOffset   -- 1 byte
  , g1CardKeyDoorX      :: !SaveOffset   -- 1 byte
  , g1TrashCanLock1     :: !SaveOffset   -- 1 byte
  , g1TrashCanLock2     :: !SaveOffset   -- 1 byte
  , g1CurrentMapScript  :: !SaveOffset   -- 1 byte
  } deriving (Show)

data Gen2SaveOffsets = Gen2SaveOffsets
  { g2PlayerName     :: !SaveOffset
  , g2RivalName      :: !SaveOffset
  , g2PartyData      :: !SaveOffset
  , g2CurrentBox     :: !SaveOffset
  , g2Checksum1      :: !SaveOffset
  , g2Checksum1Start :: !SaveOffset
  , g2Checksum1End   :: !SaveOffset
  , g2Checksum2      :: !SaveOffset
  , g2Checksum2Start :: !SaveOffset
  , g2Checksum2End   :: !SaveOffset
  , g2BoxBanks       :: ![BoxBankInfo]
  } deriving (Show)

data BoxBankInfo = BoxBankInfo
  { bankStartOffset  :: !SaveOffset
  , bankBoxCount     :: !Int
  , bankBoxDataSize  :: !Int
  , bankAllChecksum  :: !SaveOffset   -- offset of the bank-wide checksum byte
  , bankBoxChecksums :: !SaveOffset   -- offset of the first per-box checksum byte (consecutive)
  } deriving (Show)


-- ── Struct Sizes ──────────────────────────────────────────────────

-- | Gen 1 container and struct sizes.
--
-- Source: Bulbapedia, "Save data structure (Generation I)"
-- Party: always 6 capacity, 44-byte party structs, 33-byte box structs
-- Box capacity varies by region: 20 (Western), 30 (Japanese)

gen1PartyCapacity :: Int
gen1PartyCapacity = 6

gen1PartyPokemonSize :: Int
gen1PartyPokemonSize = 44

gen1BoxPokemonSize :: Int
gen1BoxPokemonSize = 33

gen1BagCapacity :: Int
gen1BagCapacity = 20

gen1PCItemCapacity :: Int
gen1PCItemCapacity = 50

gen1HoFRecordCount :: Int
gen1HoFRecordCount = 50

gen1HoFSlotsPerRecord :: Int
gen1HoFSlotsPerRecord = 6

-- | HoF entry size in bytes. May differ for Japanese saves —
-- needs verification when adding that layout.
gen1HoFEntrySize :: Int
gen1HoFEntrySize = 16

gen1HoFRecordSize :: Int
gen1HoFRecordSize = gen1HoFSlotsPerRecord * gen1HoFEntrySize


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
  , layoutNameLen     = NameLength 11
  , layoutOffsets     = Gen1Offsets westernGen1Offsets
  , layoutBoxCount    = 12
  , layoutBoxCapacity = BoxCapacity 20
  }

-- | Western Gen 1 save file offsets. Shared across Red, Blue, and Yellow.
--
-- Source: Bulbapedia, "Save data structure (Generation I)"
-- https://bulbapedia.bulbagarden.net/wiki/Save_data_structure_(Generation_I)
--
-- Hall of Fame:          Bank 0, 0x0598 (50 records × 6 entries × 16 bytes)
-- Player name:           Bank 1, 0x2598 (11 bytes)
-- Pokedex owned:         Bank 1, 0x25A3 (19 bytes, bit-packed)
-- Pokedex seen:          Bank 1, 0x25B6 (19 bytes, bit-packed)
-- Bag items:             Bank 1, 0x25C9 (count + pairs + 0xFF)
-- Money:                 Bank 1, 0x25F3 (3 bytes BCD)
-- Rival name:            Bank 1, 0x25F6 (11 bytes)
-- Options:               Bank 1, 0x2601 (1 byte)
-- Badges:                Bank 1, 0x2602 (1 byte bitfield)
-- Letter delay:          Bank 1, 0x2604 (1 byte)
-- Player ID:             Bank 1, 0x2605 (2 bytes big-endian)
-- Music ID:              Bank 1, 0x2607 (1 byte)
-- Music bank:            Bank 1, 0x2608 (1 byte)
-- Contrast ID:           Bank 1, 0x2609 (1 byte)
-- Current map:           Bank 1, 0x260A (1 byte)
-- Player Y:              Bank 1, 0x260D (1 byte)
-- Player X:              Bank 1, 0x260E (1 byte)
-- Last map:              Bank 1, 0x2611 (1 byte)
-- Pikachu happiness:     Bank 1, 0x271B (1 byte, Yellow only)
-- Pikachu mood:          Bank 1, 0x271C (1 byte, Yellow only)
-- Surfing hi-score:      Bank 1, 0x2740 (2 bytes little-endian BCD, Yellow only)
-- Printer settings:      Bank 1, 0x2743 (1 byte, Yellow only)
-- PC box items:          Bank 1, 0x27E6 (count + pairs + 0xFF)
-- Current box number:    Bank 1, 0x284C (1 byte)
-- Hall of Fame count:    Bank 1, 0x284E (1 byte)
-- Casino coins:          Bank 1, 0x2850 (2 bytes BCD)
-- Toggle flags:          Bank 1, 0x2852 (32 bytes, 256-bit flag array)
-- Map scripts:           Bank 1, 0x289C (256 bytes, per-map script progress)
-- Hidden items:          Bank 1, 0x299C (14 bytes)
-- Hidden coins:          Bank 1, 0x29AA (2 bytes)
-- Movement status:       Bank 1, 0x29AC (1 byte)
-- Towns visited:         Bank 1, 0x29B7 (2 bytes bitfield)
-- Safari steps:          Bank 1, 0x29B9 (2 bytes big-endian)
-- Fossil item:           Bank 1, 0x29BB (1 byte)
-- Fossil result:         Bank 1, 0x29BC (3 bytes)
-- Enemy trainer class:   Bank 1, 0x29BF (1 byte)
-- Rival starter:         Bank 1, 0x29C1 (1 byte, internal species index)
-- Player starter:        Bank 1, 0x29C3 (1 byte, internal species index)
-- Boulder sprite index:  Bank 1, 0x29C4 (1 byte)
-- Last blackout map:     Bank 1, 0x29C5 (1 byte)
-- Destination map:       Bank 1, 0x29C6 (1 byte)
-- Dungeon warp dest:     Bank 1, 0x29C9 (1 byte)
-- Dungeon warp used:     Bank 1, 0x29CA (1 byte)
-- Var flags 1:           Bank 1, 0x29D4 (1 byte)
-- Defeated gyms:         Bank 1, 0x29D6 (1 byte bitfield)
-- Var flags 2:           Bank 1, 0x29D8 (1 byte)
-- Var flags 3:           Bank 1, 0x29D9 (1 byte)
-- Var flags 4:           Bank 1, 0x29DA (1 byte)
-- Var flags 5:           Bank 1, 0x29DC (1 byte)
-- Var flags 6:           Bank 1, 0x29DE (1 byte)
-- Var flags 7:           Bank 1, 0x29DF (1 byte)
-- Defeated Lorelei:      Bank 1, 0x29E0 (2 bytes)
-- Var flags 8:           Bank 1, 0x29E2 (1 byte)
-- In-game trades:        Bank 1, 0x29E3 (2 bytes bitset)
-- Warped from warp:      Bank 1, 0x29E7 (1 byte)
-- Warped from map:       Bank 1, 0x29E8 (1 byte)
-- Card key door Y:       Bank 1, 0x29EB (1 byte)
-- Card key door X:       Bank 1, 0x29EC (1 byte)
-- Trash can lock 1:      Bank 1, 0x29EF (1 byte)
-- Trash can lock 2:      Bank 1, 0x29F0 (1 byte)
-- Event flags:           Bank 1, 0x29F3 (320 bytes, 2560-bit flag array)
-- Current map script:    Bank 1, 0x2CE5 (1 byte)
-- Play time:             Bank 1, 0x2CED (5 bytes: hours, maxed, min, sec, frames)
-- Safari game over:      Bank 1, 0x2CF2 (1 byte)
-- Safari ball count:     Bank 1, 0x2CF3 (1 byte)
-- Daycare in use:        Bank 1, 0x2CF4 (1 byte)
-- Daycare nickname:      Bank 1, 0x2CF5 (11 bytes, raw text)
-- Daycare OT name:       Bank 1, 0x2D00 (11 bytes, raw text)
-- Daycare pokemon:           Bank 1, 0x2D0B (1 byte, internal species index)
-- Party data:            Bank 1, 0x2F2C (0x194 bytes)
-- Current box:           Bank 1, 0x30C0 (0x462 bytes)
-- Checksum:              Bank 1, 0x3523 (1 byte)
-- Checksum range:        0x2598-0x3522 (complement of byte sum)
-- Box banks:             Bank 2 at 0x4000, Bank 3 at 0x6000 (6 boxes each, 0x462 per box)
-- Bank 2 checksum:       0x5A4C (1 byte, complement of byte sum over 0x4000-0x5A4B)
-- Bank 2 box checks:     0x5A4D (6 bytes, one per box)
-- Bank 3 checksum:       0x7A4C (1 byte, complement of byte sum over 0x6000-0x7A4B)
-- Bank 3 box checks:     0x7A4D (6 bytes, one per box)
westernGen1Offsets :: Gen1SaveOffsets
westernGen1Offsets = Gen1SaveOffsets
  { g1HallOfFame        = SaveOffset 0x0598
  , g1PlayerName        = SaveOffset 0x2598
  , g1ChecksumStart     = SaveOffset 0x2598
  , g1PokedexOwned      = SaveOffset 0x25A3
  , g1PokedexSeen       = SaveOffset 0x25B6
  , g1BagItems          = SaveOffset 0x25C9
  , g1Money             = SaveOffset 0x25F3
  , g1RivalName         = SaveOffset 0x25F6
  , g1Options           = SaveOffset 0x2601
  , g1Badges            = SaveOffset 0x2602
  , g1LetterDelay       = SaveOffset 0x2604
  , g1PlayerID          = SaveOffset 0x2605
  , g1MusicId           = SaveOffset 0x2607
  , g1MusicBank         = SaveOffset 0x2608
  , g1ContrastId        = SaveOffset 0x2609
  , g1CurrentMap        = SaveOffset 0x260A
  , g1PlayerY           = SaveOffset 0x260D
  , g1PlayerX           = SaveOffset 0x260E
  , g1LastMap           = SaveOffset 0x2611
  , g1PikachuHappiness  = SaveOffset 0x271B
  , g1PikachuMood       = SaveOffset 0x271C
  , g1SurfingHiScore    = SaveOffset 0x2740
  , g1PrinterSettings   = SaveOffset 0x2743
  , g1BoxItems          = SaveOffset 0x27E6
  , g1CurrentBoxNumber  = SaveOffset 0x284C
  , g1HoFCount          = SaveOffset 0x284E
  , g1CasinoCoins       = SaveOffset 0x2850
  , g1ToggleFlags       = SaveOffset 0x2852
  , g1MapScripts        = SaveOffset 0x289C
  , g1HiddenItems       = SaveOffset 0x299C
  , g1HiddenCoins       = SaveOffset 0x29AA
  , g1MovementStatus    = SaveOffset 0x29AC
  , g1TownsVisited      = SaveOffset 0x29B7
  , g1SafariSteps       = SaveOffset 0x29B9
  , g1FossilItem        = SaveOffset 0x29BB
  , g1FossilResult      = SaveOffset 0x29BC
  , g1EnemyTrainerClass = SaveOffset 0x29BF
  , g1RivalStarter      = SaveOffset 0x29C1
  , g1PlayerStarter     = SaveOffset 0x29C3
  , g1BoulderSpriteIndex = SaveOffset 0x29C4
  , g1LastBlackoutMap   = SaveOffset 0x29C5
  , g1DestinationMap    = SaveOffset 0x29C6
  , g1DungeonWarpDest   = SaveOffset 0x29C9
  , g1DungeonWarpUsed   = SaveOffset 0x29CA
  , g1VarFlags1         = SaveOffset 0x29D4
  , g1DefeatedGyms      = SaveOffset 0x29D6
  , g1VarFlags2         = SaveOffset 0x29D8
  , g1VarFlags3         = SaveOffset 0x29D9
  , g1VarFlags4         = SaveOffset 0x29DA
  , g1VarFlags5         = SaveOffset 0x29DC
  , g1VarFlags6         = SaveOffset 0x29DE
  , g1VarFlags7         = SaveOffset 0x29DF
  , g1DefeatedLorelei   = SaveOffset 0x29E0
  , g1VarFlags8         = SaveOffset 0x29E2
  , g1InGameTrades      = SaveOffset 0x29E3
  , g1WarpedFromWarp    = SaveOffset 0x29E7
  , g1WarpedFromMap     = SaveOffset 0x29E8
  , g1CardKeyDoorY      = SaveOffset 0x29EB
  , g1CardKeyDoorX      = SaveOffset 0x29EC
  , g1TrashCanLock1     = SaveOffset 0x29EF
  , g1TrashCanLock2     = SaveOffset 0x29F0
  , g1EventFlags        = SaveOffset 0x29F3
  , g1CurrentMapScript  = SaveOffset 0x2CE5
  , g1PlayTime          = SaveOffset 0x2CED
  , g1SafariGameOver    = SaveOffset 0x2CF2
  , g1SafariBallCount   = SaveOffset 0x2CF3
  , g1DaycareInUse      = SaveOffset 0x2CF4
  , g1DaycareNickname   = SaveOffset 0x2CF5
  , g1DaycareOTName     = SaveOffset 0x2D00
  , g1DaycarePokemon    = SaveOffset 0x2D0B
  , g1PartyData         = SaveOffset 0x2F2C
  , g1CurrentBox        = SaveOffset 0x30C0
  , g1ChecksumEnd       = SaveOffset 0x3522
  , g1Checksum          = SaveOffset 0x3523
  , g1BoxBanks          =
      [ BoxBankInfo { bankStartOffset = SaveOffset 0x4000, bankBoxCount = 6, bankBoxDataSize = 1122
                    , bankAllChecksum = SaveOffset 0x5A4C, bankBoxChecksums = SaveOffset 0x5A4D }
      , BoxBankInfo { bankStartOffset = SaveOffset 0x6000, bankBoxCount = 6, bankBoxDataSize = 1122
                    , bankAllChecksum = SaveOffset 0x7A4C, bankBoxChecksums = SaveOffset 0x7A4D }
      ]
  }

-- ── Yellow Pikachu overworld state (not parsed) ─────────────
--
-- Yellow fills a 128-byte reserved block with Pikachu AI state:
-- overworld flags, spawn state, movement command buffer, expression
-- and animation numbers, movement script pointers. Some of these
-- may be transient (movement buffers) while others may affect
-- game behavior on reload (spawn state, overworld flags). We
-- haven't verified which fields the game reads back on load vs
-- which it reinitializes.
--
-- The meaningful fields from this block (happiness, mood, surfing
-- hi-score, printer settings) are parsed as individual fields.
-- The rest can be added if a use case emerges.

-- ── Intentionally not parsed ────────────────────────────────
--
-- These fields describe the current map's rendering state: tile
-- layout, VRAM pointers, sprite graphics, warp geometry, and sign
-- positions. They are saved and restored by the game as part of
-- the full WRAM dump, but most are reloaded from ROM map data
-- when entering a new map. Editing them may have no effect (if
-- the map reloads the values) or unpredictable effects (if the
-- game uses the saved value before reloading).
--
-- They are preserved by the round-trip serializer (patch-only
-- approach) but are not exposed as parsed fields. If a use case
-- emerges for any of these, they can be added as raw bytes
-- without architecture changes.
--
-- 0x260B  UL Corner Tile Block Map Ptr (2 bytes)
-- 0x260F  Y Block Coord (1 byte)
-- 0x2610  X Block Coord (1 byte)
-- 0x2613  Current Tileset (1 byte)
-- 0x2614  Map Height Blocks (1 byte)
-- 0x2615  Map Width Blocks (1 byte)
-- 0x2616  Map Data Pointer (2 bytes)
-- 0x2618  Map Text Pointer (2 bytes)
-- 0x261A  Map Script Pointer (2 bytes)
-- 0x261C  Map Connections (1 byte)
-- 0x261D  Map Connection data (44 bytes, 4 × 11)
-- 0x2649  Sprite Set IDs (12 bytes)
-- 0x2655  Object Data Pointers Tmp (4 bytes)
-- 0x2659  Out of Bounds Tile (1 byte)
-- 0x265A  Warp Count + Warp Entries (129 bytes)
-- 0x26DB  Warp Destination ID (1 byte)
-- 0x275C  Sign Count + Sign Coords + Sign Text IDs (49 bytes)
-- 0x278D  Sprite Count (1 byte)
-- 0x278E  Y/X Offset since last special warp (2 bytes)
-- 0x2790  Map Sprite Data (32 bytes)
-- 0x27B0  Map Sprite Extra Data (32 bytes)
-- 0x27D0  Map 2x2 Meta Height/Width (2 bytes)
-- 0x27D2  Map View VRAM Pointer (2 bytes)
-- 0x27D4  Player Movement/Direction state (3 bytes)
-- 0x27D7  Tileset Bank + pointers + tile data (10 bytes)
-- 0x2879  Scratch byte (1 byte)
-- 0x287A  Missable List runtime structure (34 bytes)
-- 0x29C0  Player Jumping Y Screen Coords (1 byte)
-- 0x2D2C  Full Sprite Data section (512 bytes)
-- 0x3522  Tileset Type (1 byte)

renderVariant :: GameVariant -> Text
renderVariant RedBlue    = "Red/Blue"
renderVariant Yellow     = "Yellow"
renderVariant GoldSilver = "Gold/Silver"
renderVariant Crystal    = "Crystal"

renderRegion :: SaveRegion -> Text
renderRegion RegionJapanese = "Japanese"
renderRegion RegionWestern  = "Western"
renderRegion RegionKorean   = "Korean"
