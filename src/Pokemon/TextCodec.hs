{-# LANGUAGE OverloadedStrings #-}

-- | Game Boy text encoding and decoding.
--
-- The Game Boy uses a custom byte↔character mapping that varies by
-- generation and language. This module loads charset data from JSON
-- files and provides encode/decode between raw bytes and GameText.
--
-- The codec is purely about byte translation. Naming screen
-- validation (which characters are choosable) is provided alongside
-- for the legality layer to consume, but the codec itself doesn't
-- enforce it.

module Pokemon.TextCodec
  ( -- * Codec type
    TextCodec (..)
  , NamingScreen (..)

    -- * Loading
  , loadCodec

    -- * Encoding / Decoding
  , terminator
  , decodeText
  , encodeText

    -- * Display
  , displayText
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Types


-- ── Types ─────────────────────────────────────────────────────────

-- | A loaded character encoding for one (Gen, Language) pair.
-- The decode and encode maps are inverses for all known characters.
data TextCodec = TextCodec
  { codecGen      :: !Gen
  , codecLanguage :: !Language
  , codecDecode   :: !(Map.Map Word8 GameChar)
  , codecEncode   :: !(Map.Map GameChar Word8)
  } deriving (Show)

-- | One game variant's naming screen — the set of characters a
-- player can type. A (Gen, Language) may have multiple screens
-- (e.g. German Gen 2 Gold/Silver vs Crystal).
data NamingScreen = NamingScreen
  { screenLabel :: !T.Text
  , screenChars :: !(Set.Set GameChar)
  } deriving (Show)


-- ── Constants ─────────────────────────────────────────────────────

-- | The string terminator byte. All Game Boy text strings end here.
-- Same value (0x50) across all gens and regions.
terminator :: Word8
terminator = 0x50


-- ── Decode / Encode ───────────────────────────────────────────────

-- | Decode raw bytes into GameText. Stops at the terminator byte
-- (0x50) or end of input. Unrecognized bytes become UnknownByte
-- values, preserving round-trip fidelity.
decodeText :: TextCodec -> BS.ByteString -> GameText
decodeText codec bs = GameText (go 0)
  where
    go i
      | i >= BS.length bs       = []
      | byte == terminator      = []
      | otherwise = case Map.lookup byte (codecDecode codec) of
          Just gc -> gc : go (i + 1)
          Nothing -> UnknownByte byte : go (i + 1)
      where byte = BS.index bs i

-- | Encode GameText into a fixed-length ByteString, padded with
-- terminators. Characters not in the encode map are dropped.
-- The output is always exactly @len@ bytes.
encodeText :: TextCodec -> Int -> GameText -> BS.ByteString
encodeText codec len (GameText chars) =
  let encoded = take (len - 1)   -- leave room for at least one terminator
        [ b | gc <- chars
            , b <- case gc of
                UnknownByte w -> [w]
                _ -> case Map.lookup gc (codecEncode codec) of
                       Just w  -> [w]
                       Nothing -> []
        ]
      padding = replicate (len - length encoded) terminator
  in BS.pack (encoded ++ padding)


-- ── Display ───────────────────────────────────────────────────────

-- | Convert GameText to human-readable Text. This is a lossy
-- conversion: Literal and Ligature expand to their text
-- representations, UnknownByte renders as a hex placeholder.
-- Use this only for final display output.
displayText :: GameText -> T.Text
displayText (GameText chars) = T.concat (map renderChar chars)
  where
    renderChar (Literal c)     = T.singleton c
    renderChar (Ligature t)    = t
    renderChar (UnknownByte w) = T.pack ("[0x" ++ showHex w ++ "]")
    showHex w =
      let h = fromIntegral w :: Int
          hi = h `div` 16
          lo = h `mod` 16
          hexDigit n
            | n < 10    = toEnum (n + fromEnum '0')
            | otherwise = toEnum (n - 10 + fromEnum 'A')
      in [hexDigit hi, hexDigit lo]


-- ── JSON Loading ──────────────────────────────────────────────────

-- | Load a TextCodec and its associated NamingScreens for a
-- (Gen, Language) pair. Reads the appropriate charset JSON file.
loadCodec :: Gen -> Language -> IO (TextCodec, [NamingScreen])
loadCodec gen lang = do
  dir <- getDataDir
  let path = dir </> "charsets" </> charsetFilename gen lang
  raw <- LBS.readFile path
  case Aeson.eitherDecode raw of
    Left err  -> error $ "Failed to parse " ++ path ++ ": " ++ err
    Right val -> pure (buildFromJSON gen lang val)


-- | Map (Gen, Language) to the charset JSON filename.
-- FR/IT/ES share the EN file in our current data. This will
-- change when we add proper per-language charset files.
charsetFilename :: Gen -> Language -> FilePath
charsetFilename gen lang = prefix ++ "-" ++ suffix ++ ".json"
  where
    prefix = case gen of
      Gen1 -> "gen1"
      Gen2 -> "gen2"
    suffix = case lang of
      English  -> "en"
      French   -> "en"   -- shared with EN (TODO: separate FR file)
      Italian  -> "en"   -- shared with EN (TODO: separate IT file)
      Spanish  -> "en"   -- shared with EN (TODO: separate ES file)
      German   -> "de"
      Japanese -> "jp"


-- ── JSON Parsing ──────────────────────────────────────────────────

-- | Build a TextCodec and NamingScreens from parsed JSON.
-- Handles both flat (characters array) and variant (German Gen 2)
-- JSON structures.
buildFromJSON :: Gen -> Language -> Aeson.Value -> (TextCodec, [NamingScreen])
buildFromJSON gen lang val =
  case parseCharsets val of
    Left err -> error $ "Charset parse error: " ++ err
    Right charsets ->
      let -- Merge all character entries for the decode/encode maps
          -- (all variants share the same encoding; only choosable differs)
          allEntries = concatMap fst charsets
          decMap = Map.fromList
            [ (entByte e, entGameChar e) | e <- allEntries ]
          encMap = Map.fromList
            [ (entGameChar e, entByte e) | e <- allEntries ]
          codec = TextCodec
            { codecGen      = gen
            , codecLanguage = lang
            , codecDecode   = decMap
            , codecEncode   = encMap
            }
          screens =
            [ NamingScreen label (Set.fromList [entGameChar e | e <- entries, entChoosable e])
            | (entries, label) <- charsets
            ]
      in (codec, screens)


-- | A parsed character entry from JSON.
data CharEntry = CharEntry
  { entByte      :: !Word8
  , entGameChar  :: !GameChar
  , entChoosable :: !Bool
  }

-- | Parse the JSON value into a list of (entries, label) pairs.
-- Flat JSON → one pair. Variant JSON (gen2-de) → one pair per variant.
parseCharsets :: Aeson.Value -> Either String [([CharEntry], T.Text)]
parseCharsets = Aeson.parseEither $ \val -> do
  obj <- Aeson.parseJSON val
  -- Check for "variants" key (gen2-de.json structure)
  case Aeson.parseMaybe (Aeson..: "variants") obj of
    Just (variants :: Aeson.Object) ->
      mapM parseVariant [(Key.toText k, v) | (k, v) <- KM.toList variants]
    Nothing -> do
      -- Flat structure: single "characters" array
      chars <- obj Aeson..: "characters"
      entries <- mapM parseCharEntry chars
      label <- do
        gen <- obj Aeson..: "generation" :: Aeson.Parser Int
        region <- obj Aeson..: "region" :: Aeson.Parser T.Text
        pure $ "Gen " <> T.pack (show gen) <> " " <> T.toUpper region
      pure [(entries, label)]
  where
    parseVariant (label, val') = do
      obj <- Aeson.parseJSON val'
      chars <- obj Aeson..: "characters"
      entries <- mapM parseCharEntry chars
      pure (entries, label)


-- | Parse one character entry from the JSON characters array.
parseCharEntry :: Aeson.Value -> Aeson.Parser CharEntry
parseCharEntry = Aeson.withObject "CharEntry" $ \obj -> do
  hexStr <- obj Aeson..: "hex" :: Aeson.Parser T.Text
  glyph  <- obj Aeson..: "glyph" :: Aeson.Parser T.Text
  let byte = parseHexByte hexStr

  -- Determine if choosable. Only explicit "choosable": false marks
  -- a character as non-choosable. The "normal" field in the JSON is
  -- ambiguous (PK/MN/♂/♀/× are "not normal" but ARE choosable),
  -- so we ignore it for this purpose.
  explicitChoosable <- obj Aeson..:? "choosable"
  let choosable = case (explicitChoosable :: Maybe Bool) of
        Just c  -> c
        Nothing -> True

  let gc = case T.length glyph of
        1 -> Literal (T.head glyph)
        _ -> Ligature glyph

  pure CharEntry
    { entByte      = byte
    , entGameChar  = gc
    , entChoosable = choosable
    }


-- | Parse "0x7F" style hex strings to Word8.
parseHexByte :: T.Text -> Word8
parseHexByte t =
  let s = T.unpack (T.drop 2 t)  -- drop "0x"
      hexVal []     = 0
      hexVal (c:cs) = fromIntegral (digitToInt c) * 16 ^ length cs + hexVal cs
      digitToInt c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
        | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
        | otherwise             = 0
  in hexVal s
