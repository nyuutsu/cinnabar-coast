{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Map.Strict as Map

import Pokemon.Types
import Pokemon.Stats
import Pokemon.Data (loadGameData)
import Pokemon.TextCodec (TextCodec (..), loadCodec, encodeText, decodeText, terminator)


-- ── Arbitrary instances ─────────────────────────────────────────

instance Arbitrary DVs where
  arbitrary = DVs <$> choose (0, 15) <*> choose (0, 15)
                  <*> choose (0, 15) <*> choose (0, 15)

instance Arbitrary StatExp where
  arbitrary = StatExp <$> choose (0, 65535) <*> choose (0, 65535)
                      <*> choose (0, 65535) <*> choose (0, 65535)
                      <*> choose (0, 65535)


-- ── Main ────────────────────────────────────────────────────────

main :: IO ()
main = hspec $ do

  -- ── Property tests ──────────────────────────────────────────

  describe "dvHP" $
    prop "returns 0-15 for any valid DVs" $ \dvs ->
      let derivedHP = dvHP dvs
      in derivedHP >= 0 && derivedHP <= 15

  describe "expForLevel" $
    prop "is monotonically increasing for levels 2-99" $
      forAll (elements [MediumFast, MediumSlow, Fast, Slow]) $ \rate ->
        all (\level -> expForLevel rate level < expForLevel rate (level + 1)) [2..99]

  describe "calcStat" $
    prop "returns positive values" $ \(dvs :: DVs) (statExp :: StatExp) ->
      forAll (choose (1, 255)) $ \base ->
      forAll (choose (1, 100)) $ \level ->
        calcStat base (dvAttack dvs) (expAttack statExp) level > 0

  describe "calcHP" $
    prop "returns positive values" $ \(dvs :: DVs) (statExp :: StatExp) ->
      forAll (choose (1, 255)) $ \base ->
      forAll (choose (1, 100)) $ \level ->
        calcHP base (dvHP dvs) (expHP statExp) level > 0

  describe "Text codec round-trip" $ do
    codec <- runIO $ fst <$> loadCodec Gen1 English
    let safeChars = [ gameChar | (gameChar, byte) <- Map.toList (codecEncode codec)
                         , byte /= terminator ]
    prop "preserves all characters in the encode map" $
      forAll (GameText <$> listOf (elements safeChars)) $ \gameText ->
        let encoded = encodeText codec 11 gameText
            decoded = decodeText codec encoded
            expected = GameText (take 10 $ gameTextChars gameText)
        in decoded === expected

  -- ── Golden tests ────────────────────────────────────────────

  describe "Pikachu Gen 1" $ do
    gameData <- runIO $ loadGameData Gen1
    let pikachu = case Map.lookup 25 (gameSpecies gameData) of
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
      let stats = calcAllStats pikachu maxDVs zeroStatExp 50
      statHP stats `shouldBe` 110
      statAttack stats `shouldBe` 75
      statDefense stats `shouldBe` 50
      statSpeed stats `shouldBe` 110
      statSpecial stats `shouldBe` Unified 70

  describe "expForLevel golden" $
    it "MediumFast level 100 equals 1000000" $
      expForLevel MediumFast 100 `shouldBe` 1000000

  describe "isShiny" $ do
    it "DVs 10 10 10 10 is shiny" $
      isShiny (DVs 10 10 10 10) `shouldBe` True

    it "DVs 15 15 15 15 is not shiny" $
      isShiny (DVs 15 15 15 15) `shouldBe` False
