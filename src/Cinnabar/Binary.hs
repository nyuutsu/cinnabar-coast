-- | Cursor-based reader for fixed-layout binary formats.
--
-- Pure functions over ByteString. No IO, no Either — this is for
-- known-good offsets in fixed-size files. Out-of-bounds reads are
-- programming bugs, not data errors.

module Cinnabar.Binary
  ( -- * Cursor
    Cursor
  , mkCursor
  , cursorOffset

    -- * Reading
  , readByte
  , readWord16BE
  , readWord24BE
  , readBytes

    -- * Writing
  , writeByte
  , writeWord16BE
  , writeWord24BE

    -- * Patching
  , patchByte
  , patchBytes

    -- * Navigation
  , seekTo
  , skip
  ) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8, Word16)


-- ── Cursor ──────────────────────────────────────────────────────

-- | A position within a ByteString. Tracks the current offset
-- for sequential reads through fixed-layout binary data.
data Cursor = Cursor
  { cursorSource :: !ByteString
  , cursorOffset :: !Int
  }

-- | Create a cursor at the beginning of a ByteString.
mkCursor :: ByteString -> Cursor
mkCursor source = Cursor { cursorSource = source, cursorOffset = 0 }


-- ── Reading ─────────────────────────────────────────────────────

-- | Read one byte and advance.
readByte :: Cursor -> (Word8, Cursor)
readByte cursor =
  let byte = ByteString.index (cursorSource cursor) (cursorOffset cursor)
  in (byte, cursor { cursorOffset = cursorOffset cursor + 1 })

-- | Read a 16-bit big-endian value and advance by 2.
readWord16BE :: Cursor -> (Word16, Cursor)
readWord16BE cursor =
  let source = cursorSource cursor
      offset = cursorOffset cursor
      highByte = fromIntegral (ByteString.index source offset) :: Word16
      lowByte  = fromIntegral (ByteString.index source (offset + 1)) :: Word16
  in (highByte `shiftL` 8 .|. lowByte, cursor { cursorOffset = offset + 2 })

-- | Read a 24-bit big-endian value as Int and advance by 3.
-- Used for experience values (0–16,777,215).
readWord24BE :: Cursor -> (Int, Cursor)
readWord24BE cursor =
  let source = cursorSource cursor
      offset = cursorOffset cursor
      highByte = fromIntegral (ByteString.index source offset)
      midByte  = fromIntegral (ByteString.index source (offset + 1))
      lowByte  = fromIntegral (ByteString.index source (offset + 2))
  in ( highByte `shiftL` 16 .|. midByte `shiftL` 8 .|. lowByte
     , cursor { cursorOffset = offset + 3 }
     )

-- | Read a slice of n bytes and advance. Zero-copy — uses
-- ByteString's take/drop which share the underlying buffer.
readBytes :: Int -> Cursor -> (ByteString, Cursor)
readBytes count cursor =
  let slice = ByteString.take count (ByteString.drop (cursorOffset cursor) (cursorSource cursor))
  in (slice, cursor { cursorOffset = cursorOffset cursor + count })


-- ── Writing ───────────────────────────────────────────────────

-- | Encode one byte. Inverse of readByte.
writeByte :: Word8 -> ByteString
writeByte = ByteString.singleton

-- | Encode a 16-bit value as 2 big-endian bytes. Inverse of readWord16BE.
writeWord16BE :: Word16 -> ByteString
writeWord16BE value = ByteString.pack
  [fromIntegral (value `shiftR` 8), fromIntegral value]

-- | Encode a 24-bit value as 3 big-endian bytes. Inverse of readWord24BE.
writeWord24BE :: Int -> ByteString
writeWord24BE value = ByteString.pack
  [fromIntegral (value `shiftR` 16), fromIntegral (value `shiftR` 8), fromIntegral value]


-- ── Patching ──────────────────────────────────────────────────

-- | Replace a single byte at the given offset.
patchByte :: Int -> Word8 -> ByteString -> ByteString
patchByte offset byte bytes =
  let (prefix, suffix) = ByteString.splitAt offset bytes
  in prefix <> ByteString.singleton byte <> ByteString.drop 1 suffix

-- | Splice a chunk into a ByteString at the given offset,
-- replacing the same number of bytes.
patchBytes :: Int -> ByteString -> ByteString -> ByteString
patchBytes offset chunk bytes =
  let (prefix, suffix) = ByteString.splitAt offset bytes
  in prefix <> chunk <> ByteString.drop (ByteString.length chunk) suffix


-- ── Navigation ────────────────────────────────────────────────

-- | Jump to an absolute offset.
seekTo :: Int -> Cursor -> Cursor
seekTo offset cursor = cursor { cursorOffset = offset }

-- | Advance by n bytes without reading.
skip :: Int -> Cursor -> Cursor
skip count cursor = cursor { cursorOffset = cursorOffset cursor + count }
