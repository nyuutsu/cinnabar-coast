-- | Raw types and struct parser for Gen 1 Pokémon data.
--
-- These types mirror the binary layout exactly. Every field is the
-- raw byte value with no interpretation. The party struct is 44 bytes;
-- the box struct is the first 33 bytes of the same layout.

module Cinnabar.Save.Gen1.Raw
  ( -- * Raw types
    RawStatExp (..)
  , RawGen1PartyPokemon (..)
  , RawGen1BoxPokemon (..)

    -- * Parsers
  , parseGen1PartyPokemon
  , parseGen1BoxPokemon
  ) where

import Data.Word (Word8, Word16)

import Cinnabar.Binary (Cursor, readByte, readWord16BE, readWord24BE)
import Cinnabar.Types (InternalIndex (..))


-- ── Raw Types ──────────────────────────────────────────────────

data RawStatExp = RawStatExp
  { rawExpHP      :: !Word16
  , rawExpAttack  :: !Word16
  , rawExpDefense :: !Word16
  , rawExpSpeed   :: !Word16
  , rawExpSpecial :: !Word16
  } deriving (Eq, Show)

data RawGen1PartyPokemon = RawGen1PartyPokemon
  { rawG1SpeciesIndex :: !InternalIndex
  , rawG1CurrentHP    :: !Word16
  , rawG1BoxLevel     :: !Word8
  , rawG1Status       :: !Word8
  , rawG1Type1        :: !Word8
  , rawG1Type2        :: !Word8
  , rawG1CatchRate    :: !Word8
  , rawG1Move1        :: !Word8
  , rawG1Move2        :: !Word8
  , rawG1Move3        :: !Word8
  , rawG1Move4        :: !Word8
  , rawG1OTID         :: !Word16
  , rawG1Exp          :: !Int       -- 24-bit big-endian; no Word24 in Haskell
  , rawG1StatExp      :: !RawStatExp
  , rawG1DVBytes      :: !Word16   -- packed: Atk[15:12] Def[11:8] Spd[7:4] Spc[3:0]
  , rawG1PP1          :: !Word8    -- bits 7-6: PP Ups, bits 5-0: current PP
  , rawG1PP2          :: !Word8
  , rawG1PP3          :: !Word8
  , rawG1PP4          :: !Word8
  , rawG1Level        :: !Word8
  , rawG1MaxHP        :: !Word16
  , rawG1Attack       :: !Word16
  , rawG1Defense      :: !Word16
  , rawG1Speed        :: !Word16
  , rawG1Special      :: !Word16
  } deriving (Eq, Show)

data RawGen1BoxPokemon = RawGen1BoxPokemon
  { rawG1BoxSpeciesIndex :: !InternalIndex
  , rawG1BoxCurrentHP    :: !Word16
  , rawG1BoxBoxLevel     :: !Word8
  , rawG1BoxStatus       :: !Word8
  , rawG1BoxType1        :: !Word8
  , rawG1BoxType2        :: !Word8
  , rawG1BoxCatchRate    :: !Word8
  , rawG1BoxMove1        :: !Word8
  , rawG1BoxMove2        :: !Word8
  , rawG1BoxMove3        :: !Word8
  , rawG1BoxMove4        :: !Word8
  , rawG1BoxOTID         :: !Word16
  , rawG1BoxExp          :: !Int    -- 24-bit big-endian; no Word24 in Haskell
  , rawG1BoxStatExp      :: !RawStatExp
  , rawG1BoxDVBytes      :: !Word16
  , rawG1BoxPP1          :: !Word8
  , rawG1BoxPP2          :: !Word8
  , rawG1BoxPP3          :: !Word8
  , rawG1BoxPP4          :: !Word8
  } deriving (Eq, Show)


-- ── Parsers ────────────────────────────────────────────────────

parseRawStatExp :: Cursor -> (RawStatExp, Cursor)
parseRawStatExp cursor0 =
  let (hp,      cursor1) = readWord16BE cursor0
      (attack,  cursor2) = readWord16BE cursor1
      (defense, cursor3) = readWord16BE cursor2
      (speed,   cursor4) = readWord16BE cursor3
      (special, cursor5) = readWord16BE cursor4
  in ( RawStatExp
        { rawExpHP      = hp
        , rawExpAttack  = attack
        , rawExpDefense = defense
        , rawExpSpeed   = speed
        , rawExpSpecial = special
        }
     , cursor5
     )

-- | Parse 33 bytes of shared box data, returning the fields and
-- the cursor positioned after byte 32 (ready for party-only fields).
parseBoxFields :: Cursor -> (RawGen1BoxPokemon, Cursor)
parseBoxFields cursor0 =
  let (speciesByte, cursor1)  = readByte cursor0
      (currentHP,   cursor2)  = readWord16BE cursor1
      (boxLevel,    cursor3)  = readByte cursor2
      (status,      cursor4)  = readByte cursor3
      (type1,       cursor5)  = readByte cursor4
      (type2,       cursor6)  = readByte cursor5
      (catchRate,   cursor7)  = readByte cursor6
      (move1,       cursor8)  = readByte cursor7
      (move2,       cursor9)  = readByte cursor8
      (move3,       cursor10) = readByte cursor9
      (move4,       cursor11) = readByte cursor10
      (otId,        cursor12) = readWord16BE cursor11
      (experience,  cursor13) = readWord24BE cursor12
      (statExp,     cursor14) = parseRawStatExp cursor13
      (dvBytes,     cursor15) = readWord16BE cursor14
      (pp1,         cursor16) = readByte cursor15
      (pp2,         cursor17) = readByte cursor16
      (pp3,         cursor18) = readByte cursor17
      (pp4,         cursor19) = readByte cursor18
  in ( RawGen1BoxPokemon
        { rawG1BoxSpeciesIndex = InternalIndex speciesByte
        , rawG1BoxCurrentHP    = currentHP
        , rawG1BoxBoxLevel     = boxLevel
        , rawG1BoxStatus       = status
        , rawG1BoxType1        = type1
        , rawG1BoxType2        = type2
        , rawG1BoxCatchRate    = catchRate
        , rawG1BoxMove1        = move1
        , rawG1BoxMove2        = move2
        , rawG1BoxMove3        = move3
        , rawG1BoxMove4        = move4
        , rawG1BoxOTID         = otId
        , rawG1BoxExp          = experience
        , rawG1BoxStatExp      = statExp
        , rawG1BoxDVBytes      = dvBytes
        , rawG1BoxPP1          = pp1
        , rawG1BoxPP2          = pp2
        , rawG1BoxPP3          = pp3
        , rawG1BoxPP4          = pp4
        }
     , cursor19
     )

parseGen1BoxPokemon :: Cursor -> (RawGen1BoxPokemon, Cursor)
parseGen1BoxPokemon = parseBoxFields

parseGen1PartyPokemon :: Cursor -> (RawGen1PartyPokemon, Cursor)
parseGen1PartyPokemon cursor0 =
  let (boxPokemon,  cursor1) = parseBoxFields cursor0
      (level,   cursor2) = readByte cursor1
      (maxHP,   cursor3) = readWord16BE cursor2
      (attack,  cursor4) = readWord16BE cursor3
      (defense, cursor5) = readWord16BE cursor4
      (speed,   cursor6) = readWord16BE cursor5
      (special, cursor7) = readWord16BE cursor6
  in ( RawGen1PartyPokemon
        { rawG1SpeciesIndex = rawG1BoxSpeciesIndex boxPokemon
        , rawG1CurrentHP    = rawG1BoxCurrentHP boxPokemon
        , rawG1BoxLevel     = rawG1BoxBoxLevel boxPokemon
        , rawG1Status       = rawG1BoxStatus boxPokemon
        , rawG1Type1        = rawG1BoxType1 boxPokemon
        , rawG1Type2        = rawG1BoxType2 boxPokemon
        , rawG1CatchRate    = rawG1BoxCatchRate boxPokemon
        , rawG1Move1        = rawG1BoxMove1 boxPokemon
        , rawG1Move2        = rawG1BoxMove2 boxPokemon
        , rawG1Move3        = rawG1BoxMove3 boxPokemon
        , rawG1Move4        = rawG1BoxMove4 boxPokemon
        , rawG1OTID         = rawG1BoxOTID boxPokemon
        , rawG1Exp          = rawG1BoxExp boxPokemon
        , rawG1StatExp      = rawG1BoxStatExp boxPokemon
        , rawG1DVBytes      = rawG1BoxDVBytes boxPokemon
        , rawG1PP1          = rawG1BoxPP1 boxPokemon
        , rawG1PP2          = rawG1BoxPP2 boxPokemon
        , rawG1PP3          = rawG1BoxPP3 boxPokemon
        , rawG1PP4          = rawG1BoxPP4 boxPokemon
        , rawG1Level        = level
        , rawG1MaxHP        = maxHP
        , rawG1Attack       = attack
        , rawG1Defense      = defense
        , rawG1Speed        = speed
        , rawG1Special      = special
        }
     , cursor7
     )
