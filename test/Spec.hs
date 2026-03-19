{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Pokemon.Types
import Pokemon.Stats
import Pokemon.Data (loadAllGameData)
import Pokemon.Error (loadOrDie)
import Pokemon.Legality (classifyMove)
import Pokemon.TextCodec (TextCodec (..), loadCodec, encodeText, decodeText, terminator)


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
    codec <- runIO $ fst <$> loadCodec Gen1 English
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
    it "DVs 10 10 10 10 is shiny" $
      isShiny (DVs 10 10 10 10) `shouldBe` True

    it "DVs 15 15 15 15 is not shiny" $
      isShiny (DVs 15 15 15 15) `shouldBe` False

  -- ── Move Legality ─────────────────────────────────────────

  describe "Move Legality" $ do
    let lookupSpeciesByName gameData name =
          case Map.lookup name (gameSpeciesByName (gameSpeciesGraph gameData)) of
            Just dex -> dex
            Nothing  -> error $ "Species not found: " ++ T.unpack name

        lookupMoveByName gameData name =
          case Map.lookup name (gameMoveByName (gameLookupTables gameData)) of
            Just matchedMoveId -> matchedMoveId
            Nothing            -> error $ "Move not found: " ++ T.unpack name

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
