{-# LANGUAGE OverloadedStrings #-}

-- | Gen-agnostic decoders for save file interpretation.
--
-- These work for Gen 1 and Gen 2: BCD, bitfields, status
-- conditions, options, Pokedex flags, named bit flags.

module Cinnabar.Save.Interpret.Decode
  ( interpretStatus
  , interpretOptions
  , yellowOnly
  , decodeBCD
  , decodeBCDLE
  , decodePokedexFlags
  , decodeNamedBitFlags
  , decodeMapScripts
  ) where

import Data.Bits (testBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word8)

import Cinnabar.Types (DexNumber (..))
import Cinnabar.Save.Layout (GameVariant (..))
import Cinnabar.Save.Interpret.Types


-- | Decode the status byte. Bits 0-2 are sleep turns, bits 3-6 are
-- PSN/BRN/FRZ/PAR (mutually exclusive). The game enforces at most one
-- condition, but a save editor could create invalid combinations.
interpretStatus :: Word8 -> StatusCondition
interpretStatus byte
  | sleepTurns /= 0 && statusBits /= 0 = MultipleStatuses byte
  | sleepTurns /= 0                     = Asleep sleepTurns
  | statusBits == 0                     = Healthy
  | statusBits == 0x08                  = Poisoned
  | statusBits == 0x10                  = Burned
  | statusBits == 0x20                  = Frozen
  | statusBits == 0x40                  = Paralyzed
  | otherwise                           = MultipleStatuses byte
  where
    sleepTurns = fromIntegral (byte .&. 0x07)
    statusBits = byte .&. 0xF8  -- bits 3-7

interpretOptions :: GameVariant -> Word8 -> InterpretedOptions
interpretOptions variant byte = InterpretedOptions
  { optTextSpeed       = case byte .&. 0x07 of
      1 -> TextFast
      3 -> TextMedium
      5 -> TextSlow
      n -> TextSpeedUnknown (fromIntegral n)
  , optBattleAnimation = if testBit byte 7 then AnimationsOff else AnimationsOn
  , optBattleStyle     = if testBit byte 6 then BattleShift else BattleSet
  , optSound           = yellowOnly variant $ case (byte .&. 0x30) `shiftR` 4 of
      0 -> Mono
      1 -> Earphone1
      2 -> Earphone2
      _ -> Earphone3
  }

yellowOnly :: GameVariant -> a -> Maybe a
yellowOnly Yellow value = Just value
yellowOnly _      _     = Nothing

-- | Decode a big-endian Binary-Coded Decimal byte string.
-- Each nybble is one decimal digit.
decodeBCD :: ByteString -> Int
decodeBCD = ByteString.foldl' addByte 0
  where
    addByte acc byte =
      acc * 100 + fromIntegral (byte `shiftR` 4) * 10 + fromIntegral (byte .&. 0x0F)

-- | Decode a 2-byte little-endian Binary-Coded Decimal value.
-- Low byte first, high byte second. Each nybble is one decimal digit.
decodeBCDLE :: ByteString -> Int
decodeBCDLE bytes =
  let low  = ByteString.index bytes 0
      high = ByteString.index bytes 1
      decodeByte byte = fromIntegral (byte `shiftR` 4) * 10 + fromIntegral (byte .&. 0x0F)
  in decodeByte low + decodeByte high * 100

-- | Decode a bit-packed Pokedex byte string into the set of flagged
-- dex numbers. Bit N (0-indexed, LSB-first within each byte) maps
-- to species N+1.
decodePokedexFlags :: ByteString -> Set DexNumber
decodePokedexFlags bytes = Set.fromList
  [ DexNumber (bitIndex + 1)
  | (byteIndex, byte) <- zip [0 ..] (ByteString.unpack bytes)
  , bitOffset <- [0 .. 7]
  , let bitIndex = byteIndex * 8 + bitOffset
  , testBit byte bitOffset
  ]

-- | Walk a bit-packed byte string and produce a FlagState for every named bit.
-- LSB-first within each byte, same layout as decodePokedexFlags.
decodeNamedBitFlags :: Map.Map Int Text -> ByteString -> [FlagState]
decodeNamedBitFlags nameMap bytes =
  [ FlagState { flagName = name, flagIsSet = isSet }
  | (bitIndex, name) <- Map.toAscList nameMap
  , let byteIndex = bitIndex `div` 8
        bitOffset = bitIndex `mod` 8
        isSet     = byteIndex < ByteString.length bytes
                 && testBit (ByteString.index bytes byteIndex) bitOffset
  ]

-- | Produce a MapScriptState for every named script offset, including step 0.
decodeMapScripts :: Map.Map Int Text -> ByteString -> [MapScriptState]
decodeMapScripts nameMap bytes =
  [ MapScriptState { scriptName = name, scriptStep = stepValue }
  | (offset, name) <- Map.toAscList nameMap
  , let stepValue
          | offset < ByteString.length bytes = fromIntegral (ByteString.index bytes offset)
          | otherwise                        = 0
  ]
