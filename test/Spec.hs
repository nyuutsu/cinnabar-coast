{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.ByteString as ByteString
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Cinnabar.Types
import Cinnabar.Stats
import Cinnabar.Data (loadAllGameData)
import Cinnabar.Error (loadOrDie)
import Cinnabar.Legality (classifyMove)
import Cinnabar.TextCodec (TextCodec (..), loadCodec, encodeText, decodeText, terminator)
import Cinnabar.Binary (mkCursor, cursorOffset, patchByte, patchBytes)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Gen1.Raw
import Cinnabar.Save.Layout
  ( GameVariant (..), SaveRegion (..), CartridgeLayout (..)
  , SaveOffsets (..), Gen1SaveOffsets (..), cartridgeLayout
  )
import Cinnabar.Save.Interpret
import Cinnabar.Save.Raw
import Cinnabar.Save.Serialize (serializeGen1Save)
import System.Directory (doesFileExist)


-- ── Arbitrary instances ─────────────────────────────────────────

instance Arbitrary DVs where
  arbitrary = DVs <$> choose (0, 15) <*> choose (0, 15)
                  <*> choose (0, 15) <*> choose (0, 15)

instance Arbitrary StatExp where
  arbitrary = StatExp <$> choose (0, 65535) <*> choose (0, 65535)
                      <*> choose (0, 65535) <*> choose (0, 65535)
                      <*> choose (0, 65535)

instance Arbitrary GrowthRate where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Level where
  arbitrary = Level <$> choose (1, 100)




-- ── Main ────────────────────────────────────────────────────────

main :: IO ()
main = hspec $ do
  (gen1Data, gen2Data) <- runIO (loadOrDie =<< loadAllGameData)

  -- ── Property tests ──────────────────────────────────────────

  describe "dvHP" $
    prop "returns 0-15 for any valid DVs" $ \dvs ->
      let derivedHP = dvHP dvs
      in derivedHP >= 0 && derivedHP <= 15

  describe "expForLevel" $
    prop "is monotonically increasing for levels 2-99" $ \rate ->
        all (\level -> expForLevel rate (Level level) < expForLevel rate (Level (level + 1))) [2..99]

  describe "calcStat" $
    prop "returns positive values" $ \(dvs :: DVs) (statExp :: StatExp) ->
      forAll (choose (1, 255)) $ \base ->
      forAll (Level <$> choose (1, 100)) $ \level ->
        calcStat (StatInput base (dvAttack dvs) (expAttack statExp)) level > 0

  describe "calcHP" $
    prop "returns positive values" $ \(dvs :: DVs) (statExp :: StatExp) ->
      forAll (choose (1, 255)) $ \base ->
      forAll (Level <$> choose (1, 100)) $ \level ->
        calcHP (StatInput base (dvHP dvs) (expHP statExp)) level > 0

  describe "Text codec round-trip" $ do
    codec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)
    let safeChars = [ gameChar | (byte, gameChar) <- Map.toList (codecDecode codec)
                         , byte /= terminator ]
    prop "preserves all characters in the decode map" $
      forAll (GameText <$> listOf (elements safeChars)) $ \gameText ->
        let encoded = encodeText 11 gameText
            decoded = decodeText codec encoded
            expected = GameText (take 10 $ gameTextChars gameText)
        in decoded === expected

  -- ── Golden tests ────────────────────────────────────────────

  describe "Pikachu Gen 1" $ do
    let pikachu = case Map.lookup (DexNumber 25) (gameSpecies (gameSpeciesGraph gen1Data)) of
          Just species -> species
          Nothing      -> error "Pikachu (dex 25) not found in Gen 1 data"

    it "has correct base stats" $ do
      let baseStats = speciesBaseStats pikachu
      baseHP baseStats `shouldBe` 35
      baseAttack baseStats `shouldBe` 55
      baseDefense baseStats `shouldBe` 30
      baseSpeed baseStats `shouldBe` 90
      baseSpecial baseStats `shouldBe` Unified 50

    it "at level 50, max DVs, zero stat exp gives correct stats" $ do
      let stats = calcAllStats pikachu maxDVs zeroStatExp (Level 50)
      statHP stats `shouldBe` 110
      statAttack stats `shouldBe` 75
      statDefense stats `shouldBe` 50
      statSpeed stats `shouldBe` 110
      statSpecial stats `shouldBe` Unified 70

  describe "expForLevel golden" $
    it "MediumFast level 100 equals 1000000" $
      expForLevel MediumFast (Level 100) `shouldBe` 1000000

  describe "isShiny" $ do
    it "accepts all 8 valid shiny Attack DVs" $
      let shinyDVs = [DVs atk 10 10 10 | atk <- [2, 3, 6, 7, 10, 11, 14, 15]]
      in all isShiny shinyDVs `shouldBe` True

    prop "matches the shiny specification" $ \dvs ->
      isShiny dvs === ( dvDefense dvs == 10
                     && dvSpeed dvs == 10
                     && dvSpecial dvs == 10
                     && dvAttack dvs `elem` [2, 3, 6, 7, 10, 11, 14, 15] )

  -- ── Internal index mapping ───────────────────────────────

  describe "Internal index mapping" $
    it "loads internal index mapping with correct known entries" $ do
      let indexMap = gameInternalIndex (gameSpeciesGraph gen1Data)
      Map.lookup (InternalIndex 1) indexMap `shouldBe` Just (DexNumber 112)
      Map.lookup (InternalIndex 84) indexMap `shouldBe` Just (DexNumber 25)
      Map.lookup (InternalIndex 31) indexMap `shouldBe` Nothing
      Map.size indexMap `shouldBe` 151

  -- ── Move Legality ─────────────────────────────────────────

  describe "Move Legality" $ do
    let lookupSpeciesByName gameData name =
          case Map.lookup name (gameSpeciesByName (gameSpeciesGraph gameData)) of
            Just dex -> dex
            Nothing  -> error $ "Species not found: " ++ Text.unpack name

        lookupMoveByName gameData name =
          case Map.lookup name (gameMoveByName (gameLookupTables gameData)) of
            Just matchedMoveId -> matchedMoveId
            Nothing            -> error $ "Move not found: " ++ Text.unpack name

        methods sources = map sourceMethod sources

    it "Pikachu learns Thunder by level-up in Gen 2" $ do
      let pikachuDex = lookupSpeciesByName gen2Data "PIKACHU"
          thunderId  = lookupMoveByName gen2Data "THUNDER"
          sources    = classifyMove gen2Data (Just gen1Data) pikachuDex thunderId (Level 100)
      LevelUp `elem` methods sources `shouldBe` True

    it "Pikachu learns Toxic by TM in Gen 2" $ do
      let pikachuDex = lookupSpeciesByName gen2Data "PIKACHU"
          toxicId    = lookupMoveByName gen2Data "TOXIC"
          sources    = classifyMove gen2Data (Just gen1Data) pikachuDex toxicId (Level 100)
      TMMachine `elem` methods sources `shouldBe` True

    it "Larvitar has Pursuit as an egg move in Gen 2" $ do
      let larvitarDex = lookupSpeciesByName gen2Data "LARVITAR"
          pursuitId   = lookupMoveByName gen2Data "PURSUIT"
          sources     = classifyMove gen2Data (Just gen1Data) larvitarDex pursuitId (Level 100)
      EggMove `elem` methods sources `shouldBe` True

    it "Snorlax learns Flamethrower by tutor in Gen 2" $ do
      let snorlaxDex    = lookupSpeciesByName gen2Data "SNORLAX"
          flamethrowerId = lookupMoveByName gen2Data "FLAMETHROWER"
          sources        = classifyMove gen2Data (Just gen1Data) snorlaxDex flamethrowerId (Level 100)
      TutorMove `elem` methods sources `shouldBe` True

    it "Chansey learns Mega Punch by tradeback to Gen 1 TM" $ do
      let chanseyDex  = lookupSpeciesByName gen2Data "CHANSEY"
          megaPunchId = lookupMoveByName gen2Data "MEGA_PUNCH"
          sources     = classifyMove gen2Data (Just gen1Data) chanseyDex megaPunchId (Level 100)
          tradebackSources = filter ((== Tradeback) . sourceMethod) sources
          nestedMethods    = concatMap (map sourceMethod . sourceVia) tradebackSources
      Tradeback `elem` methods sources `shouldBe` True
      TMMachine `elem` nestedMethods `shouldBe` True

    it "Raichu learns Thunder Wave via pre-evolution in Gen 2" $ do
      let raichuDex     = lookupSpeciesByName gen2Data "RAICHU"
          thunderWaveId = lookupMoveByName gen2Data "THUNDER_WAVE"
          sources       = classifyMove gen2Data (Just gen1Data) raichuDex thunderWaveId (Level 100)
      PreEvo `elem` methods sources `shouldBe` True

    it "Pikachu cannot learn Aeroblast" $ do
      let pikachuDex  = lookupSpeciesByName gen2Data "PIKACHU"
          aeroblastId = lookupMoveByName gen2Data "AEROBLAST"
          sources     = classifyMove gen2Data (Just gen1Data) pikachuDex aeroblastId (Level 100)
      sources `shouldBe` []

    it "Pikachu at level 10 can learn Thunder by TM but not level-up" $ do
      let pikachuDex = lookupSpeciesByName gen2Data "PIKACHU"
          thunderId  = lookupMoveByName gen2Data "THUNDER"
          sources    = classifyMove gen2Data (Just gen1Data) pikachuDex thunderId (Level 10)
      LevelUp `elem` methods sources `shouldBe` False
      TMMachine `elem` methods sources `shouldBe` True

  -- ── Save file infrastructure ────────────────────────────────

  describe "calculateGen1Checksum" $
    it "produces complement of byte sum" $
      let saveBytes = ByteString.pack [0x01, 0x02, 0x03]
      in calculateGen1Checksum saveBytes 0 2 `shouldBe` 0xF9

  describe "cartridgeLayout" $
    it "returns layouts for implemented games" $ do
      cartridgeLayout Yellow RegionWestern `shouldSatisfy` isRight
      cartridgeLayout GoldSilver RegionJapanese `shouldSatisfy` isLeft

  describe "Gen 1 struct parsers" $
    it "parseGen1PartyMon and parseGen1BoxMon parse a hand-crafted struct" $ do
      let pikachuBytes = ByteString.pack
            [ 0x54                          -- species: Pikachu internal index
            , 0x00, 0x64                    -- current HP: 100
            , 0x19                          -- box level: 25
            , 0x00                          -- status: none
            , 0x17, 0x17                    -- type1, type2: Electric
            , 0xBE                          -- catch rate: 190
            , 0x54, 0x55, 0x56, 0x57        -- moves
            , 0x30, 0x39                    -- OT ID: 12345
            , 0x00, 0xC3, 0x50             -- experience: 50000 (24-bit BE)
            , 0x00, 0x00, 0x00, 0x00, 0x00  -- stat exp HP, Attack, Defense
            , 0x00, 0x00, 0x00, 0x00, 0x00  -- stat exp Speed, Special
            , 0xFA, 0xD8                    -- DVs: 0xFAD8
            , 0x23, 0x24, 0x25, 0x26        -- PP slots
            -- box struct ends here (33 bytes)
            , 0x19                          -- party level: 25
            , 0x00, 0x6E                    -- max HP: 110
            , 0x00, 0x4B                    -- attack: 75
            , 0x00, 0x32                    -- defense: 50
            , 0x00, 0x6E                    -- speed: 110
            , 0x00, 0x46                    -- special: 70
            ]
          (partyMon, partyCursor) = parseGen1PartyMon (mkCursor pikachuBytes)
          (boxMon, boxCursor)     = parseGen1BoxMon (mkCursor pikachuBytes)
      rawG1SpeciesIndex partyMon `shouldBe` InternalIndex 0x54
      rawG1Exp partyMon `shouldBe` 50000
      rawG1DVBytes partyMon `shouldBe` 0xFAD8
      rawG1OTID partyMon `shouldBe` 12345
      rawG1Level partyMon `shouldBe` 25
      rawG1BoxSpeciesIndex boxMon `shouldBe` rawG1SpeciesIndex partyMon
      cursorOffset partyCursor `shouldBe` 44
      cursorOffset boxCursor `shouldBe` 33

  describe "Save file parser" $ do
    it "parses a real Yellow save file" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                rawGen1ChecksumValid save `shouldBe` True
                let party = rawGen1Party save
                    count = fromIntegral (rawGen1PartyCount party)
                count `shouldSatisfy` (\n -> n >= 1 && n <= (6 :: Int))
                length (rawGen1PartySpecies party) `shouldBe` count
                length (rawGen1PartyMons party) `shouldBe` count

    it "parses an empty party without crashing" $
      case cartridgeLayout Yellow RegionWestern of
        Left msg -> expectationFailure (Text.unpack msg)
        Right layout -> case layoutOffsets layout of
          Gen2Offsets _ -> expectationFailure "expected Gen 1 offsets"
          Gen1Offsets offsets -> do
            let zeroes = ByteString.replicate 32768 0x00
                withTerminators = patchByte (g1PartyData offsets + 1) 0xFF
                                $ patchByte (g1CurrentBox offsets + 1) 0xFF
                                $ zeroes
                checksum = calculateGen1Checksum withTerminators
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
                saveBytes = patchByte (g1Checksum offsets) checksum withTerminators
            case parseRawSave layout saveBytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                let party = rawGen1Party save
                rawGen1PartyCount party `shouldBe` 0
                rawGen1PartySpecies party `shouldBe` []
                rawGen1PartyMons party `shouldBe` []

    it "parses trainer profile fields at known offsets" $
      case cartridgeLayout Yellow RegionWestern of
        Left msg -> expectationFailure (Text.unpack msg)
        Right layout -> case layoutOffsets layout of
          Gen2Offsets _ -> expectationFailure "expected Gen 1 offsets"
          Gen1Offsets offsets -> do
            let base = ByteString.replicate 32768 0x00
                withFields = patchBytes (g1Money offsets)
                               (ByteString.pack [0x12, 0x34, 0x56])
                           $ patchByte (g1Badges offsets) 0xA5
                           $ patchByte (g1PokedexOwned offsets) 0x03
                           $ patchByte (g1PlayTime offsets) 0x2A
                           $ patchByte (g1PlayTime offsets + 2) 0x1E
                           $ patchByte (g1PartyData offsets + 1) 0xFF
                           $ patchByte (g1CurrentBox offsets + 1) 0xFF
                           $ base
                checksum = calculateGen1Checksum withFields
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
                saveBytes = patchByte (g1Checksum offsets) checksum withFields
            case parseRawSave layout saveBytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                rawGen1Money save `shouldBe` ByteString.pack [0x12, 0x34, 0x56]
                rawGen1Badges save `shouldBe` 0xA5
                ByteString.index (rawGen1PokedexOwned save) 0 `shouldBe` 0x03
                rawPlayHours (rawGen1PlayTime save) `shouldBe` 0x2A
                rawPlayMinutes (rawGen1PlayTime save) `shouldBe` 0x1E

  -- ── Save interpretation ─────────────────────────────────────

  describe "Save interpretation" $ do
    codec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)

    it "interprets a hand-crafted Gen 1 save correctly" $
      case cartridgeLayout Yellow RegionWestern of
        Left msg -> expectationFailure (Text.unpack msg)
        Right layout -> case layoutOffsets layout of
          Gen2Offsets _ -> expectationFailure "expected Gen 1 offsets"
          Gen1Offsets offsets -> do
            -- Pikachu party struct: level 50, max DVs, zero stat exp
            -- Stats match the golden test: HP=110, Atk=75, Def=50, Spd=110, Spc=70
            let pikachuStruct = ByteString.pack
                  [ 0x54                          -- species: Pikachu internal index
                  , 0x00, 0x6E                    -- current HP: 110
                  , 0x32                          -- box level: 50
                  , 0x00                          -- status: none
                  , 0x17, 0x17                    -- type1, type2: Electric
                  , 0xBE                          -- catch rate: 190
                  , 0x01, 0x00, 0x00, 0x00        -- moves: Pound, empty, empty, empty
                  , 0x30, 0x39                    -- OT ID: 12345
                  , 0x01, 0xE8, 0x48              -- experience: 125000
                  , 0x00, 0x00, 0x00, 0x00, 0x00  -- stat exp: HP, Attack, Defense
                  , 0x00, 0x00, 0x00, 0x00, 0x00  -- stat exp: Speed, Special
                  , 0xFF, 0xFF                    -- DVs: all 15s
                  , 0x23, 0x00, 0x00, 0x00        -- PP: Pound=35, rest empty
                  -- box struct ends here (33 bytes)
                  , 0x32                          -- party level: 50
                  , 0x00, 0x6E                    -- max HP: 110
                  , 0x00, 0x4B                    -- attack: 75
                  , 0x00, 0x32                    -- defense: 50
                  , 0x00, 0x6E                    -- speed: 110
                  , 0x00, 0x46                    -- special: 70
                  ]
                partyOffset = g1PartyData offsets
                structStart = partyOffset + 1 + 7   -- past count + species list
                otNameStart = structStart + 6 * 44   -- past 6 mon structs
                nickStart   = otNameStart + 6 * 11   -- past 6 OT names

                base = ByteString.replicate 32768 0x00
                withParty = patchByte partyOffset 0x01                 -- count: 1
                          $ patchByte (partyOffset + 1) 0x54           -- species list: Pikachu
                          $ patchByte (partyOffset + 2) 0xFF           -- species list terminator
                          $ patchBytes structStart pikachuStruct
                          $ patchByte otNameStart 0x50                 -- OT name terminator
                          $ patchByte nickStart 0x50                   -- nickname terminator
                          $ patchByte (g1CurrentBox offsets + 1) 0xFF  -- empty box terminator
                          -- Box bank checksums (complement of all-zeroes = 0xFF)
                          $ patchBytes 0x5A4C (ByteString.replicate 7 0xFF)
                          $ patchBytes 0x7A4C (ByteString.replicate 7 0xFF)
                          $ base
                checksum = calculateGen1Checksum withParty
                             (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
                saveBytes = patchByte (g1Checksum offsets) checksum withParty

            case parseRawSave layout saveBytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save rawSave) -> do
                let interpreted = interpretGen1Save gen1Data codec rawSave
                    monList     = interpParty interpreted
                length monList `shouldBe` 1
                case monList of
                  [] -> expectationFailure "expected one party member"
                  (mon : _) -> do
                    case interpSpecies mon of
                      KnownSpecies (DexNumber dex) _ -> dex `shouldBe` 25
                      other -> expectationFailure $ "expected KnownSpecies, got: " ++ show other
                    any isKnownMove (interpMoves mon) `shouldBe` True
                    case interpSpecial mon of
                      Unified _ -> pure ()
                      Split _ _ -> expectationFailure "expected Unified special stat"
                    filter (/= ActiveBoxDesync) (interpWarnings interpreted) `shouldBe` []
            where
              isKnownMove (KnownMove _ _) = True
              isKnownMove _               = False

  -- ── Interpreted trainer profile ──────────────────────────

  describe "Interpreted trainer profile" $ do
    profileCodec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)

    it "decodes profile fields from a real Yellow save" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save rawSave) -> do
                let interpreted = interpretGen1Save gen1Data profileCodec rawSave
                interpMoney interpreted `shouldSatisfy` (>= 0)
                interpBadges interpreted `shouldSatisfy` (not . null)
                Set.size (interpPokedexOwned interpreted) `shouldSatisfy` (> 0)
                case interpPikachuFriend interpreted of
                  Just _  -> pure ()
                  Nothing -> expectationFailure "expected Pikachu friendship for Yellow save"

  -- ── PC box bank parsing ──────────────────────────────────

  describe "PC box bank parsing" $
    it "parses all 12 boxes with valid bank checksums and round-trips" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                length (rawGen1PCBoxes save) `shouldBe` 12
                map bankChecksumValid (rawGen1BoxBankValid save) `shouldBe` [True, True]
                serializeGen1Save save `shouldBe` bytes

  -- ── PC box interpretation ────────────────────────────────

  describe "PC box interpretation" $ do
    boxCodec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)

    it "interprets PC boxes with valid checksums from a real Yellow save" $ do
      let hasKnownSpecies mon = case interpSpecies mon of
            KnownSpecies _ _ -> True
            _                -> False
          isBoxWarning (BoxBankChecksumMismatch {}) = True
          isBoxWarning (BoxChecksumMismatch {})     = True
          isBoxWarning _                            = False
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save rawSave) -> do
                let interpreted = interpretGen1Save gen1Data boxCodec rawSave
                interpPCBoxes interpreted `shouldSatisfy` (not . null)
                case interpPCBoxes interpreted of
                  [] -> expectationFailure "expected non-empty PC boxes"
                  (firstBox : _) ->
                    any hasKnownSpecies (interpBoxMons firstBox) `shouldBe` True
                let boxWarnings = filter isBoxWarning (interpWarnings interpreted)
                boxWarnings `shouldBe` []

  -- ── Hall of Fame ────────────────────────────────────────────

  describe "Hall of Fame" $ do
    hofCodec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)

    it "parses all records and interprets valid entries" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                length (rawGen1HallOfFame save) `shouldBe` 50
                let interpreted = interpretGen1Save gen1Data hofCodec save
                length (interpHallOfFame interpreted) `shouldBe` interpHoFCount interpreted
                case interpHallOfFame interpreted of
                  (firstRecord : _) -> do
                    let hasKnownSpecies entry = case hofSpecies entry of
                          KnownSpecies _ _ -> True
                          _                -> False
                    any hasKnownSpecies (hofEntries firstRecord) `shouldBe` True
                  [] -> pure ()

  -- ── Progress flags ───────────────────────────────────────

  describe "Progress flags" $
    it "parses event flags and player starter from a real Yellow save" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) -> do
                ByteString.length (rawEventFlags (rawGen1Progress save)) `shouldBe` 320
                rawPlayerStarter (rawGen1Progress save) `shouldSatisfy` (/= InternalIndex 0)

  -- ── Progress interpretation ──────────────────────────────

  describe "Progress interpretation" $ do
    progressCodec <- runIO $ fst <$> (loadOrDie =<< loadCodec Gen1 English)

    it "interprets progress flags from a real Yellow save" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save rawSave) -> do
                let interpreted = interpretGen1Save gen1Data progressCodec rawSave
                    progress = interpProgress interpreted
                    isKnownStarterSpecies (KnownSpecies _ _) = True
                    isKnownStarterSpecies _ = False
                progPlayerStarter progress `shouldSatisfy` isKnownStarterSpecies
                progEventFlags progress `shouldSatisfy` (not . null)
                progReceivedStarter progress `shouldBe` True

  -- ── Serialization round-trip ──────────────────────────────

  describe "Gen 1 serialize round-trip" $
    it "parse then serialize produces identical bytes" $ do
      let savePath = "test/data/yellow.sav"
      exists <- doesFileExist savePath
      if not exists
        then pendingWith "test/data/yellow.sav not present"
        else do
          bytes <- ByteString.readFile savePath
          case cartridgeLayout Yellow RegionWestern of
            Left msg -> expectationFailure (Text.unpack msg)
            Right layout -> case parseRawSave layout bytes of
              Left err -> expectationFailure (show err)
              Right (RawGen2Save _) -> expectationFailure "expected Gen 1 save"
              Right (RawGen1Save save) ->
                serializeGen1Save save `shouldBe` bytes
