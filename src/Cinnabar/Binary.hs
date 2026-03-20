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

    -- * Navigation
  , seekTo
  , skip
  ) where

import Data.Bits (shiftL, (.|.))
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


-- ── Navigation ──────────────────────────────────────────────────

-- | Jump to an absolute offset.
seekTo :: Int -> Cursor -> Cursor
seekTo offset cursor = cursor { cursorOffset = offset }

-- | Advance by n bytes without reading.
skip :: Int -> Cursor -> Cursor
skip count cursor = cursor { cursorOffset = cursorOffset cursor + count }
