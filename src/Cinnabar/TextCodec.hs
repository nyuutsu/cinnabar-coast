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

module Cinnabar.TextCodec
  ( -- * Codec type
    TextCodec (..)
  , NamingScreen (..)
  , LoadedCodec (..)

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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception (IOException, try)
import Data.Char (toUpper)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import System.FilePath ((</>))

import Paths_cinnabar_coast (getDataDir)
import Cinnabar.Error (LoadError (..))
import Cinnabar.Types
import Cinnabar.Types.Internal (GameChar (Literal, Ligature, UnknownByte))


-- ── Types ─────────────────────────────────────────────────────────

-- | A loaded character encoding for one (Gen, Language) pair.
-- Encoding doesn't need a map — the byte is embedded in each GameChar.
data TextCodec = TextCodec
  { codecGen      :: !Gen
  , codecLanguage :: !Language
  , codecDecode   :: !(Map Word8 GameChar)
  } deriving (Show)

-- | One game variant's naming screen — the set of characters a
-- player can type. A (Gen, Language) may have multiple screens
-- (e.g. German Gen 2 Gold/Silver vs Crystal).
data NamingScreen = NamingScreen
  { screenLabel :: !Text
  , screenChars :: !(Set GameChar)
  } deriving (Show)

-- | A loaded codec paired with its naming screens.
data LoadedCodec = LoadedCodec
  { loadedTextCodec     :: !TextCodec
  , loadedNamingScreens :: ![NamingScreen]
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
decodeText :: TextCodec -> ByteString -> GameText
decodeText codec bytes = GameText (decodeFrom 0)
  where
    decodeFrom offset
      | offset >= ByteString.length bytes = []
      | byte == terminator        = []
      | otherwise = case Map.lookup byte (codecDecode codec) of
          Just gameChar -> gameChar : decodeFrom (offset + 1)
          Nothing       -> UnknownByte byte : decodeFrom (offset + 1)
      where byte = ByteString.index bytes offset

-- | Encode GameText into a fixed-length ByteString, padded with
-- terminators. The byte is extracted directly from each GameChar —
-- no lookup needed. The output is always exactly @outputLength@ bytes.
encodeText :: Int -> GameText -> ByteString
encodeText outputLength (GameText chars) =
  let encoded = take (outputLength - 1) (map charByte chars)
      padding = replicate (outputLength - length encoded) terminator
  in ByteString.pack (encoded ++ padding)


-- ── Display ───────────────────────────────────────────────────────

-- | Convert GameText to human-readable Text. This is a lossy
-- conversion: Literal and Ligature expand to their text
-- representations, UnknownByte renders as a hex placeholder.
-- Use this only for final display output.
displayText :: GameText -> Text
displayText (GameText chars) = Text.concat (map renderChar chars)
  where
    renderChar (Literal _sourceByte char)    = Text.singleton char
    renderChar (Ligature _sourceByte text)   = text
    renderChar (UnknownByte rawByte)         = Text.pack ("[0x" ++ showHexByte rawByte ++ "]")

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
lookupLigature :: TextCodec -> Text -> Maybe GameChar
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
loadCodec :: Gen -> Language -> IO (Either [LoadError] LoadedCodec)
loadCodec gen lang = do
  dataDir <- getDataDir
  let path = dataDir </> "charsets" </> charsetFilename gen lang
  readResult <- try (LazyByteString.readFile path)
  case readResult of
    Left ioException ->
      pure $ Left [CharsetParseError path (Text.pack (show (ioException :: IOException)))]
    Right rawJson -> case Aeson.eitherDecode rawJson of
      Left decodeError -> pure $ Left
        [CharsetParseError path (Text.pack decodeError)]
      Right jsonValue -> pure $ buildFromJSON path gen lang jsonValue


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
buildFromJSON :: FilePath -> Gen -> Language -> Aeson.Value -> Either [LoadError] LoadedCodec
buildFromJSON path gen lang jsonValue = do
  charsets <- wrapParseError (parseCharsets jsonValue)
  mapM_ (\variant -> checkDuplicateBytes path (variantEntries variant) (variantLabel variant)) charsets
  -- All variants share the same encoding; only choosable differs
  let allEntries = concatMap variantEntries charsets
      decodeMap = Map.fromList
        [ (entryByte entry, entryGameChar entry) | entry <- allEntries ]
      codec = TextCodec
        { codecGen      = gen
        , codecLanguage = lang
        , codecDecode   = decodeMap
        }
      screens =
        [ NamingScreen (variantLabel variant)
            (Set.fromList [entryGameChar entry | entry <- variantEntries variant, entryChoosable entry])
        | variant <- charsets
        ]
  pure (LoadedCodec codec screens)
  where
    wrapParseError (Left message) = Left [CharsetParseError path (Text.pack message)]
    wrapParseError (Right value)  = Right value


-- | Fail if any byte value appears more than once in a single variant's entries.
checkDuplicateBytes :: FilePath -> [CharEntry] -> Text -> Either [LoadError] ()
checkDuplicateBytes path entries label =
  let byteCounts = Map.fromListWith (+)
        [(entryByte entry, 1 :: Int) | entry <- entries]
      duplicateBytes = Map.keys (Map.filter (> 1) byteCounts)
  in case duplicateBytes of
    []    -> pure ()
    (_:_) -> Left [CharsetParseError path $
               "duplicate byte values in charset variant "
               <> label <> ": "
               <> Text.unwords (map (\byte -> Text.pack ("0x" ++ showHexByte byte)) duplicateBytes)]


-- | A parsed character entry from JSON.
data CharEntry = CharEntry
  { entryByte      :: !Word8
  , entryGameChar  :: !GameChar
  , entryChoosable :: !Bool
  }

-- | One charset variant — a group of character entries with a label.
-- Flat JSONs produce a single variant; multi-variant JSONs (German
-- Gen 2) produce one per game version.
data CharsetVariant = CharsetVariant
  { variantEntries :: ![CharEntry]
  , variantLabel   :: !Text
  }

-- | Parse the JSON value into a list of charset variants.
-- Flat JSON → one variant. Variant JSON (gen2-de) → one per game version.
parseCharsets :: Aeson.Value -> Either String [CharsetVariant]
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
        region <- jsonObject Aeson..: "region" :: Aeson.Parser Text
        pure $ "Gen " <> Text.pack (show gen) <> " " <> Text.toUpper region
      pure [CharsetVariant entries label]
  where
    parseVariant (label, variantValue) = do
      jsonObject <- Aeson.parseJSON variantValue
      chars <- jsonObject Aeson..: "characters"
      entries <- mapM parseCharEntry chars
      pure (CharsetVariant entries label)


-- | Parse one character entry from the JSON characters array.
parseCharEntry :: Aeson.Value -> Aeson.Parser CharEntry
parseCharEntry = Aeson.withObject "CharEntry" $ \jsonObject -> do
  hexStr <- jsonObject Aeson..: "hex" :: Aeson.Parser Text
  glyph  <- jsonObject Aeson..: "glyph" :: Aeson.Parser Text
  byte   <- parseHexByte hexStr

  -- Determine if choosable. Only explicit "choosable": false marks
  -- a character as non-choosable. The "normal" field in the JSON is
  -- ambiguous (PK/MN/♂/♀/× are "not normal" but ARE choosable),
  -- so we ignore it for this purpose.
  explicitChoosable <- jsonObject Aeson..:? "choosable"
  let choosable = case (explicitChoosable :: Maybe Bool) of
        Just isChoosable -> isChoosable
        Nothing          -> True

  let gameChar = case Text.uncons glyph of
        Just (char, rest) | Text.null rest -> Literal byte char
        _ -> Ligature byte glyph

  pure CharEntry
    { entryByte      = byte
    , entryGameChar  = gameChar
    , entryChoosable = choosable
    }


-- | Parse "0x7F" style hex strings to Word8.
-- Uses 'fail' so parse errors are captured by the enclosing Aeson parser.
parseHexByte :: Text -> Aeson.Parser Word8
parseHexByte hexText =
  case readHex (Text.unpack (Text.drop 2 hexText)) of
    [(byteValue, "")] -> pure byteValue
    _                 -> fail $ "Invalid hex byte: " ++ Text.unpack hexText
