-- | Cursor-based reader for fixed-layout binary formats.
--
-- Safe reads via StateT that return Either SaveError, suitable for
-- untrusted input. Write and patch functions remain pure (they
-- operate on known-good data from a successful parse).

module Cinnabar.Binary
  ( -- * Errors
    SaveError (..)

    -- * Cursor
  , Cursor
  , buildCursor
  , cursorOffset

    -- * Parser
  , Parser
  , runParser

    -- * Reading
  , readByte
  , readByteAt
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
  , patchSlots
  , applyPatches

    -- * Navigation
  , seek
  , skip
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, evalStateT)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (sortOn)
import Data.Text (Text)
import Data.Word (Word8, Word16)

import Cinnabar.Types (Gen (..))


-- ── Errors ────────────────────────────────────────────────────

data SaveError
  = WrongFileSize !Int !Int
  | UnsupportedLayout !Text
  | UnimplementedGen !Gen
  | CursorOverrun !Int !Int !Int  -- offset, bytes needed, bytes available
  deriving (Eq, Show)


-- ── Cursor ──────────────────────────────────────────────────────

-- | A position within a ByteString. Tracks the current offset
-- for sequential reads through fixed-layout binary data.
data Cursor = Cursor
  { cursorSource :: !ByteString
  , cursorOffset :: !Int
  , cursorLength :: !Int
  }

-- | Create a cursor at the beginning of a ByteString.
buildCursor :: ByteString -> Cursor
buildCursor source = Cursor
  { cursorSource = source
  , cursorOffset = 0
  , cursorLength = ByteString.length source
  }


-- ── Parser ──────────────────────────────────────────────────────

-- | A parser that threads a Cursor through a computation,
-- failing with SaveError on out-of-bounds reads.
type Parser a = StateT Cursor (Either SaveError) a

-- | Run a parser on raw bytes, returning either an error or the result.
runParser :: Parser a -> ByteString -> Either SaveError a
runParser parser bytes = evalStateT parser (buildCursor bytes)


-- ── Reading ─────────────────────────────────────────────────────

-- | Read one byte and advance.
readByte :: Parser Word8
readByte = do
  cursor <- get
  let offset = cursorOffset cursor
  when (offset >= cursorLength cursor) $
    lift $ Left (CursorOverrun offset 1 (cursorLength cursor))
  let byte = ByteString.index (cursorSource cursor) offset
  put cursor { cursorOffset = offset + 1 }
  pure byte

-- | Read one byte at an absolute offset without a cursor.
-- For standalone bounds-checked reads outside a Parser context.
readByteAt :: Int -> ByteString -> Either SaveError Word8
readByteAt offset bytes
  | offset < 0 || offset >= ByteString.length bytes =
      Left (CursorOverrun offset 1 (ByteString.length bytes))
  | otherwise = Right (ByteString.index bytes offset)

-- | Read a 16-bit big-endian value and advance by 2.
readWord16BE :: Parser Word16
readWord16BE = do
  cursor <- get
  let offset = cursorOffset cursor
  when (offset + 2 > cursorLength cursor) $
    lift $ Left (CursorOverrun offset 2 (cursorLength cursor))
  let source = cursorSource cursor
      highByte = fromIntegral (ByteString.index source offset) :: Word16
      lowByte  = fromIntegral (ByteString.index source (offset + 1)) :: Word16
  put cursor { cursorOffset = offset + 2 }
  pure (highByte `shiftL` 8 .|. lowByte)

-- | Read a 24-bit big-endian value as Int and advance by 3.
-- Used for experience values (0–16,777,215).
readWord24BE :: Parser Int
readWord24BE = do
  cursor <- get
  let offset = cursorOffset cursor
  when (offset + 3 > cursorLength cursor) $
    lift $ Left (CursorOverrun offset 3 (cursorLength cursor))
  let source = cursorSource cursor
      highByte = fromIntegral (ByteString.index source offset)
      midByte  = fromIntegral (ByteString.index source (offset + 1))
      lowByte  = fromIntegral (ByteString.index source (offset + 2))
  put cursor { cursorOffset = offset + 3 }
  pure (highByte `shiftL` 16 .|. midByte `shiftL` 8 .|. lowByte)

-- | Read a slice of n bytes and advance. Zero-copy — uses
-- ByteString's take/drop which share the underlying buffer.
readBytes :: Int -> Parser ByteString
readBytes count = do
  cursor <- get
  let offset = cursorOffset cursor
  when (offset + count > cursorLength cursor) $
    lift $ Left (CursorOverrun offset count (cursorLength cursor))
  let slice = ByteString.take count (ByteString.drop offset (cursorSource cursor))
  put cursor { cursorOffset = offset + count }
  pure slice


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

-- | Patch a list of items at evenly-spaced positions within a region.
-- Each item is placed at start + index * stride, leaving all other
-- bytes in the region untouched.
patchSlots :: Int -> Int -> [ByteString] -> ByteString -> ByteString
patchSlots start stride items bytes =
  foldr (\(index, item) current -> patchBytes (start + index * stride) item current)
    bytes (zip [0..] items)

-- | Apply a batch of non-overlapping patches in a single left-to-right pass.
-- Each patch is (offset, replacement). Patches are sorted by offset internally,
-- then the result is built with ByteString Builder — O(n + k log k) instead
-- of O(n × k) for chained patchByte/patchBytes calls.
applyPatches :: [(Int, ByteString)] -> ByteString -> ByteString
applyPatches patches original =
  LazyByteString.toStrict
    (Builder.toLazyByteString (buildPatched (sortOn fst patches) 0))
  where
    buildPatched [] position =
      Builder.byteString (ByteString.drop position original)
    buildPatched ((offset, replacement) : rest) position =
      Builder.byteString (ByteString.take (offset - position) (ByteString.drop position original))
      <> Builder.byteString replacement
      <> buildPatched rest (offset + ByteString.length replacement)


-- ── Navigation ────────────────────────────────────────────────

-- | Jump to an absolute offset.
seek :: Int -> Parser ()
seek offset = do
  cursor <- get
  when (offset < 0 || offset > cursorLength cursor) $
    lift $ Left (CursorOverrun offset 0 (cursorLength cursor))
  put cursor { cursorOffset = offset }

-- | Advance by n bytes without reading.
skip :: Int -> Parser ()
skip count = do
  cursor <- get
  let newOffset = cursorOffset cursor + count
  when (newOffset < 0 || newOffset > cursorLength cursor) $
    lift $ Left (CursorOverrun (cursorOffset cursor) count (cursorLength cursor))
  put cursor { cursorOffset = newOffset }
