{-# LANGUAGE OverloadedStrings #-}

-- | Item resolution for Gen 1 saves.
--
-- Maps raw item IDs to display names, with special handling for
-- TM/HM items that carry move information.

module Cinnabar.Save.Interpret.Gen1.Items
  ( resolveItems
  , resolveMachineItem
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Data.Map.Strict as Map

import Cinnabar.Types
import Cinnabar.TextCodec (showHexByte)
import Cinnabar.Save.Raw (RawItemEntry (..))
import Cinnabar.Save.Interpret.Types


resolveItems
  :: Map.Map ItemId Text
  -> Map.Map Machine MoveId
  -> Map.Map MoveId Move
  -> [RawItemEntry]
  -> [InventoryEntry]
resolveItems itemMap machineMap moveMap = map resolveEntry
  where
    resolveEntry entry =
      let itemByte = rawItemId entry
          itemId = ItemId (fromIntegral itemByte)
          itemName = case Map.lookup itemId itemMap of
            Just name -> name
            Nothing   -> resolveMachineItem machineMap moveMap itemByte
      in InventoryEntry { entryName = itemName, entryQuantity = fromIntegral (rawItemQuantity entry) }

-- | Try to resolve an item byte as a TM/HM. Falls back to "Unknown"
-- for bytes outside the machine range.
resolveMachineItem :: Map.Map Machine MoveId -> Map.Map MoveId Move -> Word8 -> Text
resolveMachineItem machineMap moveMap byte
  | byte >= 0xC4, byte <= 0xC8 =
      let machineNumber = fromIntegral byte - 0xC4 + 1
      in formatMachine "HM" machineNumber (HM (MachineNumber machineNumber))
  | byte >= 0xC9, byte <= 0xFA =
      let machineNumber = fromIntegral byte - 0xC9 + 1
      in formatMachine "TM" machineNumber (TM (MachineNumber machineNumber))
  | otherwise = Text.pack ("Unknown [0x" ++ showHexByte byte ++ "]")
  where
    formatMachine :: Text -> Int -> Machine -> Text
    formatMachine prefix machineNumber machine =
      let label = prefix <> Text.pack (padMachineNumber machineNumber)
      in case Map.lookup machine machineMap >>= (`Map.lookup` moveMap) of
          Just move -> label <> " \xFF0F " <> moveName move
          Nothing   -> label

    padMachineNumber :: Int -> String
    padMachineNumber machineNumber
      | machineNumber < 10    = "0" ++ show machineNumber
      | otherwise = show machineNumber
