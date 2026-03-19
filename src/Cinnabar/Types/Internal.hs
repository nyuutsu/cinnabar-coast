-- | Internal constructors for GameChar.
--
-- This module exports GameChar's constructors for use by the codec
-- layer (Cinnabar.TextCodec), which needs to build GameChars from
-- charset JSON data. All other modules should import Cinnabar.Types,
-- which re-exports GameChar opaquely — type name and charByte only,
-- no constructors.
--
-- This is the standard Internal module pattern used by containers,
-- text, bytestring, etc.

module Cinnabar.Types.Internal
  ( GameChar (..)
  , charByte
  ) where

import Data.Text (Text)
import Data.Word (Word8)


-- | One character decoded from Game Boy text. Each constructor
-- carries the source byte from the ROM, making round-trip encoding
-- lossless by construction — encoding just extracts the embedded byte.
--
-- Eq and Ord include the byte, which is correct: two GameChars from
-- different bytes are different characters even if they display the
-- same glyph (e.g. dialogue period 0xE8 vs naming screen period 0xF2).
data GameChar
  = Literal     !Word8 !Char   -- ^ Single Unicode character with source byte
  | Ligature    !Word8 !Text   -- ^ Multi-char glyph ("PK", "MN", "'d", etc.)
  | UnknownByte !Word8         -- ^ Unrecognized byte, preserved for round-tripping
  deriving (Eq, Ord, Show)

-- | Extract the ROM source byte from a GameChar.
charByte :: GameChar -> Word8
charByte (Literal sourceByte _)  = sourceByte
charByte (Ligature sourceByte _) = sourceByte
charByte (UnknownByte sourceByte) = sourceByte
