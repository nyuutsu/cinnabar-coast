-- | Checksum calculation for Gen 1/2 save files.
--
-- Pure functions. Gen 1 uses complement-of-byte-sum. Gen 2 uses
-- a 16-bit variant (stubbed until the Gen 2 parser is built).

module Cinnabar.Save.Checksum
  ( calculateGen1Checksum
  , calculateGen2Checksum
  ) where

import Data.Bits (complement)
import qualified Data.ByteString as ByteString
import Data.Word (Word8, Word16)


-- ── Gen 1 ────────────────────────────────────────────────────────

-- | Gen 1 checksum: sum all bytes in the range, complement the
-- result. The range is inclusive on both ends.
calculateGen1Checksum :: ByteString.ByteString -> Int -> Int -> Word8
calculateGen1Checksum saveBytes startOffset endOffset =
  let slice = ByteString.take (endOffset - startOffset + 1) (ByteString.drop startOffset saveBytes)
  in complement (ByteString.foldl' (+) 0 slice)


-- ── Gen 2 ────────────────────────────────────────────────────────

-- | Gen 2 checksum. Signature only — implementation comes with
-- the Gen 2 save parser.
calculateGen2Checksum :: ByteString.ByteString -> Int -> Int -> Word16
calculateGen2Checksum _saveBytes _startOffset _endOffset =
  error "calculateGen2Checksum: not yet implemented"
