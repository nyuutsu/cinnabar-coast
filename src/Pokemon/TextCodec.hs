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

    -- * Lookup
  , lookupChar
  , lookupLigature

    -- * Display
  , displayText
  , showHexByte
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
import Control.Exception (IOException, try)
import Data.Char (toUpper)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Pokemon.Error (LoadError (..))
import Pokemon.Types
import Pokemon.Types.Internal (GameChar (Literal, Ligature, UnknownByte))


-- ── Types ─────────────────────────────────────────────────────────

-- | A loaded character encoding for one (Gen, Language) pair.
-- Encoding doesn't need a map — the byte is embedded in each GameChar.
data TextCodec = TextCodec
  { codecGen      :: !Gen
  , codecLanguage :: !Language
  , codecDecode   :: !(Map.Map Word8 GameChar)
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
decodeText codec bytes = GameText (decodeFrom 0)
  where
    decodeFrom offset
      | offset >= BS.length bytes = []
      | byte == terminator        = []
      | otherwise = case Map.lookup byte (codecDecode codec) of
          Just gameChar -> gameChar : decodeFrom (offset + 1)
          Nothing       -> UnknownByte byte : decodeFrom (offset + 1)
      where byte = BS.index bytes offset

-- | Encode GameText into a fixed-length ByteString, padded with
-- terminators. The byte is extracted directly from each GameChar —
-- no lookup needed. The output is always exactly @outputLength@ bytes.
encodeText :: Int -> GameText -> BS.ByteString
encodeText outputLength (GameText chars) =
  let encoded = take (outputLength - 1) (map charByte chars)
      padding = replicate (outputLength - length encoded) terminator
  in BS.pack (encoded ++ padding)


-- ── Display ───────────────────────────────────────────────────────

-- | Convert GameText to human-readable Text. This is a lossy
-- conversion: Literal and Ligature expand to their text
-- representations, UnknownByte renders as a hex placeholder.
-- Use this only for final display output.
displayText :: GameText -> T.Text
displayText (GameText chars) = T.concat (map renderChar chars)
  where
    renderChar (Literal _sourceByte char)    = T.singleton char
    renderChar (Ligature _sourceByte text)   = text
    renderChar (UnknownByte rawByte)         = T.pack ("[0x" ++ showHexByte rawByte ++ "]")

-- | Format a Word8 as a 2-character uppercase hex string.
showHexByte :: Word8 -> String
showHexByte byte = case showHex byte "" of
  [hexDigit] -> ['0', toUpper hexDigit]
  hexStr     -> map toUpper hexStr


-- ── Lookup ──────────────────────────────────────────────────────────

-- | Find a Literal GameChar in the codec by its display character.
-- When multiple bytes map to the same display character (e.g. the
-- two period bytes), returns whichever the Map iteration encounters
-- first. For user input, prefer the naming screen character set
-- which gives the correct byte for typeable characters.
lookupChar :: TextCodec -> Char -> Maybe GameChar
lookupChar codec target =
  let matches = [ gameChar
                | gameChar@(Literal _sourceByte char) <- Map.elems (codecDecode codec)
                , char == target
                ]
  in case matches of
    (found : _) -> Just found
    []          -> Nothing

-- | Find a Ligature GameChar in the codec by its display text.
lookupLigature :: TextCodec -> T.Text -> Maybe GameChar
lookupLigature codec target =
  let matches = [ gameChar
                | gameChar@(Ligature _sourceByte text) <- Map.elems (codecDecode codec)
                , text == target
                ]
  in case matches of
    (found : _) -> Just found
    []          -> Nothing


-- ── JSON Loading ──────────────────────────────────────────────────

-- | Load a TextCodec and its associated NamingScreens for a
-- (Gen, Language) pair. Reads the appropriate charset JSON file.
loadCodec :: Gen -> Language -> IO (Either [LoadError] (TextCodec, [NamingScreen]))
loadCodec gen lang = do
  dataDir <- getDataDir
  let path = dataDir </> "charsets" </> charsetFilename gen lang
  readResult <- try (LBS.readFile path)
  case readResult of
    Left ioException ->
      pure $ Left [CharsetParseError path (T.pack (show (ioException :: IOException)))]
    Right rawJson -> case Aeson.eitherDecode rawJson of
      Left decodeError -> pure $ Left
        [CharsetParseError path (T.pack decodeError)]
      Right jsonValue -> case buildFromJSON gen lang jsonValue of
        Left parseError -> pure $ Left
          [CharsetParseError path (T.pack parseError)]
        Right result -> pure $ Right result


-- | Map (Gen, Language) to the charset JSON filename.
-- Three Western encoding groups: EN, FR+DE, IT+ES.
-- Files are named for the full language group they serve.
charsetFilename :: Gen -> Language -> FilePath
charsetFilename gen lang = prefix ++ "-" ++ suffix ++ ".json"
  where
    prefix = case gen of
      Gen1 -> "gen1"
      Gen2 -> "gen2"
    suffix = case lang of
      English  -> "en"
      French   -> "frde"
      German   -> "frde"
      Italian  -> "ites"
      Spanish  -> "ites"
      Japanese -> "jp"


-- ── JSON Parsing ──────────────────────────────────────────────────

-- | Build a TextCodec and NamingScreens from parsed JSON.
-- Handles both flat (characters array) and variant (German Gen 2)
-- JSON structures.
buildFromJSON :: Gen -> Language -> Aeson.Value -> Either String (TextCodec, [NamingScreen])
buildFromJSON gen lang jsonValue = do
  charsets <- parseCharsets jsonValue
  mapM_ (\(entries, label) -> checkDuplicateBytes entries label) charsets
  -- All variants share the same encoding; only choosable differs
  let allEntries = concatMap fst charsets
      decodeMap = Map.fromList
        [ (entryByte entry, entryGameChar entry) | entry <- allEntries ]
      codec = TextCodec
        { codecGen      = gen
        , codecLanguage = lang
        , codecDecode   = decodeMap
        }
      screens =
        [ NamingScreen label (Set.fromList [entryGameChar entry | entry <- entries, entryChoosable entry])
        | (entries, label) <- charsets
        ]
  pure (codec, screens)


-- | Fail if any byte value appears more than once in a single variant's entries.
checkDuplicateBytes :: [CharEntry] -> T.Text -> Either String ()
checkDuplicateBytes entries variantLabel =
  let byteCounts = Map.fromListWith (+)
        [(entryByte entry, 1 :: Int) | entry <- entries]
      duplicateBytes = Map.keys (Map.filter (> 1) byteCounts)
  in case duplicateBytes of
    []    -> pure ()
    (_:_) -> Left $ "duplicate byte values in charset variant "
               ++ T.unpack variantLabel ++ ": "
               ++ unwords (map (\byte -> "0x" ++ showHexByte byte) duplicateBytes)


-- | A parsed character entry from JSON.
data CharEntry = CharEntry
  { entryByte      :: !Word8
  , entryGameChar  :: !GameChar
  , entryChoosable :: !Bool
  }

-- | Parse the JSON value into a list of (entries, label) pairs.
-- Flat JSON → one pair. Variant JSON (gen2-de) → one pair per variant.
parseCharsets :: Aeson.Value -> Either String [([CharEntry], T.Text)]
parseCharsets = Aeson.parseEither $ \jsonValue -> do
  jsonObject <- Aeson.parseJSON jsonValue
  -- Check for "variants" key (gen2-de.json structure)
  case Aeson.parseMaybe (Aeson..: "variants") jsonObject of
    Just (variants :: Aeson.Object) ->
      mapM parseVariant [(Key.toText key, variantJson) | (key, variantJson) <- KM.toList variants]
    Nothing -> do
      -- Flat structure: single "characters" array
      chars <- jsonObject Aeson..: "characters"
      entries <- mapM parseCharEntry chars
      label <- do
        gen <- jsonObject Aeson..: "generation" :: Aeson.Parser Int
        region <- jsonObject Aeson..: "region" :: Aeson.Parser T.Text
        pure $ "Gen " <> T.pack (show gen) <> " " <> T.toUpper region
      pure [(entries, label)]
  where
    parseVariant (label, variantValue) = do
      jsonObject <- Aeson.parseJSON variantValue
      chars <- jsonObject Aeson..: "characters"
      entries <- mapM parseCharEntry chars
      pure (entries, label)


-- | Parse one character entry from the JSON characters array.
parseCharEntry :: Aeson.Value -> Aeson.Parser CharEntry
parseCharEntry = Aeson.withObject "CharEntry" $ \jsonObject -> do
  hexStr <- jsonObject Aeson..: "hex" :: Aeson.Parser T.Text
  glyph  <- jsonObject Aeson..: "glyph" :: Aeson.Parser T.Text
  byte   <- parseHexByte hexStr

  -- Determine if choosable. Only explicit "choosable": false marks
  -- a character as non-choosable. The "normal" field in the JSON is
  -- ambiguous (PK/MN/♂/♀/× are "not normal" but ARE choosable),
  -- so we ignore it for this purpose.
  explicitChoosable <- jsonObject Aeson..:? "choosable"
  let choosable = case (explicitChoosable :: Maybe Bool) of
        Just isChoosable -> isChoosable
        Nothing          -> True

  let gameChar = case T.uncons glyph of
        Just (char, rest) | T.null rest -> Literal byte char
        _ -> Ligature byte glyph

  pure CharEntry
    { entryByte      = byte
    , entryGameChar  = gameChar
    , entryChoosable = choosable
    }


-- | Parse "0x7F" style hex strings to Word8.
-- Uses 'fail' so parse errors are captured by the enclosing Aeson parser.
parseHexByte :: T.Text -> Aeson.Parser Word8
parseHexByte hexText =
  case readHex (T.unpack (T.drop 2 hexText)) of
    [(byteValue, "")] -> pure byteValue
    _                 -> fail $ "Invalid hex byte: " ++ T.unpack hexText
